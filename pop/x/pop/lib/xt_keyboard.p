/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_keyboard.p
 > Purpose:         Xt Keyboard Handling
 > Author:          Adrian Howard, Jan 20 1992
 > Documentation:   REF *XT_KEYBOARD
 > Related Files:   LIB *FAST_XT_KEYBOARD
 */

compile_mode: pop11 +strict;


/*-----------------------------------------------------------------------*/


section;

include xpt_xtypes.ph;
include xpt_constants.ph;
include xpt_generaltypes.ph;

shadowclass constant XptKeySymPtr #_< [props ^XDT_KEYSYMPTR] >_#
        {:XptKeySym};
shadowclass constant XptModifiersPtr #_< [props ^XDT_MODIFIERSPTR] >_#
        {:XptModifiers};
shadowclass constant XptKeyCodePtr #_< [props ^XDT_KEYCODEPTR] >_#
        {:XptKeyCode};
shadowclass constant XptKeyCodeList :XptKeyCode[];
shadowclass constant XptKeyCodeListPtr #_< [props ^XDT_KEYCODELISTPTR] >_#
        {:exptr.:XptKeyCode[]};

endsection;


/*-----------------------------------------------------------------------*/


section $-Xpt =>
    XtConvertCase,
    XptConvertCase,
    XtGetKeysymTable,
    XptStackKeycodeKeysyms,
    XtKeysymToKeycodeList,
    XtRegisterCaseConverter,
    XptDefaultCaseConverter,
    XtSetKeyTranslator,
    XptDefaultKeyTranslator,
    XtTranslateKey,
    XptTranslateKey,
    XtTranslateKeycode,
    XptTranslateKeycode,
    XtGetActionKeysym,
    XptGetActionKeysym
;


uses xpt_typecheck.p;
uses fast_xt_keyboard;
uses xpt_general.p;


;;; Determine upper/lower case versions of a keysym - 22/11/91
;;; Input - <DISPLAYPTR> <KEYSYM> <KEYSYMPTR> <KEYSYMPTR>
define global XtConvertCase(display, keysym, lower, upper);
    lvars display, keysym, lower, upper;
    fast_XtConvertCase(
        XptCheckDisplayPtr(display),
        XptCheckKeySym(keysym),
        XptCheckKeySymPtr(lower),
        XptCheckKeySymPtr(upper)
    );
    refreshXptKeySymPtr(lower)->;
    refreshXptKeySymPtr(upper)->;
enddefine;


;;; Determine upper/lower case versions of a keysym. "Pop" version of
;;; -XtConvertCase- - 14/01/92
;;; Input - <DISPLAYPTR> <KEYSYM>  Output - <KEYSYM> <KEYSYM>
define global XptConvertCase(display, keysym);
    lvars display, keysym;
    fast_XptConvertCase(
        XptCheckDisplayPtr(display),
        XptCheckKeySym(keysym)
    );
enddefine;


;;; Get the keysym-to-keycode table for a display - 02/12/91
;;; Input - <DISPLAYPTR> <KEYCODEPTR> <INTPTR> Output - <KEYSYMTABLE>
define global XtGetKeysymTable(display, min_keycode, num_keysyms);
    lvars display, min_keycode, num_keysyms;
    fast_XtGetKeysymTable(
        XptCheckDisplayPtr(display),
        XptCheckKeyCodePtr(min_keycode),
        XptCheckIntPtr(num_keysyms)
    );
    refreshXptKeyCodePtr(min_keycode) ->;
    refreshXptIntPtr(num_keysyms) ->;
enddefine;


;;; Stack the keysyms associated with a given keycode on a given display.
;;; If the optional argument -refresh- is "true" then the keysym-to-keycode
;;; table for the display is refreshed.
;;; Input - <DISPLAYPTR> <KEYCODE> [<BOOL>] Output - <KEYSYM> ...
define global XptStackKeycodeKeysyms(display, keycode, /*[refresh]*/);
    lvars display, keycode, refresh = false;
    lvars table, index, num_keysyms;

    ;;; CHECK FOR OPTIONAL BOOLEAN ARGUMENT
    if isboolean(keycode) then
        (display, keycode) -> (display, keycode, refresh);
    endif;

    XptCheckDisplayPtr(display) ->;
    XptCheckKeyCode(keycode) ->;
    GetKeysymTableIndexForKeycode(display, keycode, refresh)
        -> (table, index, num_keysyms);

    if index >0 then
        StackKeysymsForKeyCode(table, index, num_keysyms);
    else
        mishap(keycode, 1, 'KEYCODE TOO SMALL');
    endif;

enddefine;


;;; Returns the keycodes referred to by a given keysym - 03/12/91
;;; NOTE: Returned KeyCodeListPtr needs to be -XtFree-'s
;;; Input - <DISPLAYPTR> <KEYSYM> <KEYCODELISTPTR> <CARDINALPTR>
define global XtKeysymToKeycodeList(
    display, keysym, keycodes_ret, keycount_ret
);
    lvars display, keysym, keycodes_ret, keycount_ret;
    fast_XtKeysymToKeycodeList(
        XptCheckDisplayPtr(display),
        XptCheckKeySym(keysym),
        XptCheckKeyCodeListPtr(keycodes_ret),
        XptCheckCardinalPtr(keycount_ret)
    );
    fillXptKeyCodeListPtr(
        importXptKeyCodeList(
            destXptKeyCodeListPtr(refreshXptKeyCodeListPtr(keycodes_ret)),
            XptCPValue(refreshXptCardinalPtr(keycount_ret))
        ),
        keycodes_ret
    ) ->;
enddefine;


;;; Register a new case converter procedure - 13/01/92
;;; Input - <DISPLAYPTR> <XtCaseProc> <KEYSYM> <KEYSYM>
define global XtRegisterCaseConverter(displayptr, caseproc, start, stop);
    lvars displayptr, caseproc, start, stop;

    fast_XtRegisterCaseConverter(
        XptCheckDisplayPtr(displayptr),
        if XptIsValidCallback(caseproc) then
            XptExportCaseProcCached(caseproc, true)
        else
            XptCheckProcedure(caseproc)
        endif,
        XptCheckKeySym(start),
        XptCheckKeySym(stop)
    );

enddefine;


;;; Reinstall default case converter procedure - 13/01/92
;;; Input - <DISPLAYPTR> <KEYSYM> <KEYSYM>
define global XptDefaultCaseConverter(displayptr, start, stop);
    lvars displayptr, start, stop;

    fast_XptDefaultCaseConverter(
        XptCheckDisplayPtr(displayptr),
        XptCheckKeySym(start),
        XptCheckKeySym(stop)
    );

enddefine;


;;; Register a new key translator - 13/01/92
;;; Input - <DISPLAYPTR> <XtKeyProc>
define global XtSetKeyTranslator(displayptr, keyproc);
    lvars displayptr, keyproc;
    fast_XtSetKeyTranslator(
        XptCheckDisplayPtr(displayptr),
        if XptIsValidCallback(keyproc) then
            XptExportKeyProcCached(keyproc, true)
        else
            XptCheckProcedure(keyproc)
        endif
    );
enddefine;


;;; Reinstall default key translator - 14/01/92
;;; Input - <DISPLAYPTR>
define global XptDefaultKeyTranslator() with_nargs 1;
    fast_XptDefaultKeyTranslator(XptCheckDisplayPtr());
enddefine;


;;; Default KeyCode to KeySym, translator - 13/01/92
;;; Input - <DISPLAYPTR> <KEYCODE> <MODIFIERS> <MODIFIERSPTR> <KEYSYMPTR>
define global XtTranslateKey(display, keycode, mods, mods_ret, keysym_ret);
    lvars display, keycode, mods, mods_ret, keysym_ret;
    fast_XtTranslateKey(
        XptCheckDisplayPtr(display),
        XptCheckKeyCode(keycode),
        XptCheckModifiers(mods),
        XptCheckModifiersPtr(mods_ret),
        XptCheckKeySymPtr(keysym_ret)
    );
    refreshXptModifiersPtr(mods_ret) ->;
    refreshXptKeySymPtr(keysym_ret) ->;
enddefine;


;;; "Pop" version of -fast_XtTranslateKey-
;;; Input - <DISPLAYPTR> <KEYCODE> <MODIFIERS>  Output - <KEYSYM> <MODIFIERS>
define global XptTranslateKey(display, keycode, mods);
    lvars display, keycode, mods;
    fast_XptTranslateKey(
        XptCheckDisplayPtr(display),
        XptCheckKeyCode(keycode),
        XptCheckModifiers(mods)
    );
enddefine;


;;; Call current KeyCode to KeySym translator - 13/01/92
;;; Input - <DISPLAYPTR> <KEYCODE> <MODIFIERS> <MODIFIERSPTR> <KEYSYMPTR>
define global XtTranslateKeycode(display, keycode, mods, mods_ret, keysym_ret);
    lvars display, keycode, mods, mods_ret, keysym_ret;
    fast_XtTranslateKeycode(
        XptCheckDisplayPtr(display),
        XptCheckKeyCode(keycode),
        XptCheckModifiers(mods),
        XptCheckModifiersPtr(mods_ret),
        XptCheckKeySymPtr(keysym_ret)
    );
    refreshXptModifiersPtr(mods_ret) ->;
    refreshXptKeySymPtr(keysym_ret) ->;
enddefine;


;;; "Pop" version of -fast_XtTranslateKeycode-
;;; Input - <DISPLAYPTR> <KEYCODE> <MODIFIERS>  Output - <KEYSYM> <MODIFIERS>
define global XptTranslateKeycode(display, keycode, mods);
    lvars display, keycode, mods;
    fast_XptTranslateKeycode(
        XptCheckDisplayPtr(display),
        XptCheckKeyCode(keycode),
        XptCheckModifiers(mods)
    );
enddefine;


;;; Get the KeySym & modifiers from an action event
;;; Input - <XEVENTPTR> <FALSE or MODIFIERSPTR>  Output - <KEYSYM>
define global XtGetActionKeysym(eventptr,modsptr);
    lvars eventptr, modsptr;
    fast_XtGetActionKeysym(
        XptCheckXEventPtr(eventptr),
        (not(modsptr) and modsptr) or XptCheckModifiersPtr(modsptr)
    );
    refreshXptModifiersPtr(modsptr)->;
enddefine;


;;; "Pop" version of -XtGetActionKeysym-
;;; Input - <XEVENTPTR>  Output - <KEYSYM> <MODIFIERS>
define global XptGetActionKeysym() with_nargs 1;
    fast_XptGetActionKeysym(XptCheckXEventPtr());
enddefine;


;;; so uses works ok
global constant xt_keyboard = true;

endsection;
