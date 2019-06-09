/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xt_keyboard.p
 > Purpose:         Xt Keyboard Handling Procedures
 > Author:          Adrian Howard, Jan 20 1992 (see revisions)
 > Documentation:   REF *XT_KEYBOARD
 > Related Files:   LIB *XT_KEYBOARD
 */

compile_mode: pop11 +strict;


section $-Xpt =>
    XptCaseProcWrapper,
    XptExportCaseProcCached,
    XptExportCaseProc,
    XptKeyProcWrapper,
    XptExportKeyProcCached,
    XptExportKeyProc,
    fast_XtConvertCase,
    fast_XptConvertCase,
    fast_XtGetKeysymTable,
    fast_XptStackKeycodeKeysyms
    fast_XtKeysymToKeycodeList,
    fast_XtRegisterCaseConverter,
    fast_XptDefaultCaseConverter,
    fast_XtSetKeyTranslator,
    fast_XptDefaultKeyTranslator,
    fast_XtTranslateKey,
    fast_XptTranslateKey,
    fast_XtTranslateKeycode,
    fast_XptTranslateKeycode,
    fast_XtGetActionKeysym,
    fast_XptGetActionKeysym
;


include xpt_generaltypes.ph;
include xpt_xtypes.ph;
include x_keysyms.ph;


;;; Load the external "raw" procedures
XptLoadProcedures fast_xt_keyboard
    lvars
        _XtConvertCase
        XtConvertCase
        XtGetKeysymTable
        XtKeysymToKeycodeList
        XtRegisterCaseConverter
        XtSetKeyTranslator
        XtTranslateKey
        XtTranslateKeycode
        XtGetActionKeysym
;

lconstant
    XptKeySym_ptr1  = EXPTRINITSTR(:XptKeySym),
    XptKeySym_ptr2  = EXPTRINITSTR(:XptKeySym),
    XptKeyCode_ptr  = EXPTRINITSTR(:XptKeyCode),
    XptModifiers_ptr= EXPTRINITSTR(:XptModifiers),
;


;;; Wrapper procedure for case converter procedures
define XptCaseProcWrapper(extdata, proc);
    lvars extdata, proc, stacked;
    l_typespec extdata {
        display :XptDisplayPtr,
        keysym  :exval.^XptKeySym,
        lower   :exptr,
        upper   :exptr
    };

    #|
        exacc [fast] extdata.display;
        exacc [fast] extdata.keysym;
        exacc [fast] extdata.lower;
        exacc [fast] extdata.upper;
        XptCallbackHandler(proc, "case_proc");
    |# -> stacked;

    unless stacked == 0 then
        warning(
            proc, stacked+1,
            'EXCESS RESULTS AFTER CASE PROC BEING IGNORED'
        );
    endunless;

enddefine;


;;; Wrapper procedure for key translator procedures
define XptKeyProcWrapper(extdata, proc);
    lvars extdata, proc, stacked;
    l_typespec extdata {
        display     :XptDisplayPtr,
        keycode     :exval.^XptKeyCode,
        mod         :exval.^XptModifiers,
        mod_ret     :exptr,
        keysym_ret  :exptr
    };

    #|
        exacc [fast] extdata.display;
        exacc [fast] extdata.keycode;
        exacc [fast] extdata.mod;
        exacc [fast] extdata.mod_ret;
        exacc [fast] extdata.keysym_ret;
        XptCallbackHandler(proc, "key_proc");
    |# -> stacked;

    unless stacked == 0 then
        warning(
            proc, stacked+1,
            'EXCESS RESULTS AFTER KEY PROC BEING IGNORED'
        );
    endunless;

enddefine;


;;; Cache, indexing closures of wrappers onto external procedures
lconstant callback_cache =
    newanyproperty( [], 50, false, false,
                    syshash, sys_=, "tmpval",
                    false, false
    );


;;; Instances of the closures used to index -callback_cache-
lconstant XptCaseProcWrapperInstance =
    writeable XptCaseProcWrapper(%false%);
lconstant XptKeyProcWrapperInstance =
    writeable XptKeyProcWrapper(%false%);


lconstant do_cached_export=
    procedure(proc, hold, wrapper_instance) -> efc;
        lvars proc, hold, wrapper_instance, efc;
        lvars callback_wrapper;
        proc -> explode(wrapper_instance);
        unless (callback_cache(wrapper_instance) ->> efc) do;
            if XptIsValidCallback(proc) then
                copy(wrapper_instance) -> callback_wrapper;
                exfunc_export(callback_wrapper, XptCallbackFlags, hold)
                    ->> callback_cache(callback_wrapper) -> efc;
            else
                mishap(proc, 1, 'PROCEDURE, IDENT, OR WORD NEEDED');
            endif;
        endunless;
    endprocedure;
;;;
define XptExportCaseProcCached() with_nargs 2;
    do_cached_export(XptCaseProcWrapperInstance);
enddefine;
;;;
define XptExportKeyProcCached() with_nargs 2;
    do_cached_export(XptKeyProcWrapperInstance);
enddefine;


lconstant do_export =
    procedure(proc, hold, wrapper); /* -> EFC */
        lvars proc, hold, wrapper;
        if XptIsValidCallback(proc) then
            exfunc_export(wrapper(%proc%), XptCallbackFlags,hold);
        else
            mishap(proc, 1, 'WORD, IDENT, OR PROCEDURE NEEDED');
        endif;
    endprocedure;
;;;
define XptExportCaseProc() with_nargs 2;
    do_export(XptCaseProcWrapper);
enddefine;
;;;
define XptExportKeyProc() with_nargs 2;
    do_export(XptKeyProcWrapper);
enddefine;


;;; Determine upper/lower case versions of a keysym - 22/11/91
;;; Input - <DISPLAYPTR> <KEYSYM> <KEYSYMPTR> <KEYSYMPTR>
define fast_XtConvertCase(display, keysym, lower, upper);
    lvars display, keysym, lower, upper;
    exacc (4) raw_XtConvertCase(display, keysym, lower, upper);
enddefine;


;;; Determine upper/lower case versions of a keysym. "Pop" version of
;;; -fast_XtConvertCase- - 14/01/92
;;; Input - <DISPLAYPTR> <KEYSYM>  Output - <KEYSYM> <KEYSYM>
define fast_XptConvertCase(display, keysym);
    lvars display, keysym;
    exacc (4) raw_XtConvertCase(display, keysym,
                                XptKeySym_ptr1, XptKeySym_ptr2);
    exacc :XptKeySym XptKeySym_ptr1;        ;;; lower
    exacc :XptKeySym XptKeySym_ptr2;        ;;; upper
enddefine;


;;; Get the keysym-to-keycode table for a display - 02/12/91
;;; Input - <DISPLAYPTR> <KEYCODEPTR> <INTPTR> Output - <KEYSYMTABLE>
define fast_XtGetKeysymTable() with_nargs 3;
    exacc (3):XptKeySymTable raw_XtGetKeysymTable();
enddefine;


;;; Association of displays to keysym tables
lconstant keysym_table_cache =
    newanyproperty( [], 10, false, false,
                    false, false, "tmparg",
                    false, false
    );


;;; Returns a keysym-to-keycode table for the given display (refreshing
;;; the cache if -refresh- is true) and returns an index into that
;;; table for the given keycode - 02/12/91
;;; Input  - <DISPLAYPTR> <KEYCODE> <BOOL>
;;; Output - <KEYSYMTABLE> <INT: index> <INT: # keysyms per keycode>
define GetKeysymTableIndexForKeycode(display, keycode, refresh);
    lvars display, keycode, refresh;
    lconstant num_keysyms_ptr = EXPTRINITSTR(:int);
    lvars cache_entry, table, min_keycode, num_keysyms;

    ;;; GET KEYCODE-TO-KEYSYM TABLE FOR THE DISPLAY
    if refresh or (keysym_table_cache(display) ->> cache_entry) == false do;
        consvector(
            exacc (3):XptKeySymTable raw_XtGetKeysymTable(
                display,
                XptKeyCode_ptr,
                num_keysyms_ptr
            ),
            exacc :XptKeyCode XptKeyCode_ptr,       ;;; min keycode
            exacc :int num_keysyms_ptr
        ,3) ->> keysym_table_cache(display) -> cache_entry;
    endif;

    explode(cache_entry) -> (table, min_keycode, num_keysyms);

    return(
        table,                                          ;;; KEYSYMTABLE
        (((keycode - min_keycode) * num_keysyms) + 1),  ;;; START INDEX
        num_keysyms                           ;;; # KEYSYMS PER KEYCODE
    );

enddefine;


;;; Stacks the keysyms for the keycode starting at the given index - 02/12/91
;;; Input - <KEYSYMTABLE> <INT: index of keycode> <INT: # keysyms per keycode>
define constant StackKeysymsForKeyCode(table, start_index, num_keysyms);
    lvars table, start_index, num_keysyms;
    lvars n = 0, keysym;
    until n == num_keysyms do;
        unless (exacc :XptKeySym[] table[start_index fi_+ n] ->> keysym)
            == XNoSymbol
        do
            keysym;
        endunless;
        n+1 -> n;
    enduntil;
enddefine;


;;; Stack the keysyms associated with a given keycode on a given display.
;;; If the optional argument -refresh- is "true" then the keysym-to-keycode
;;; table for the display is refreshed.
;;; Input - <DISPLAYPTR> <KEYCODE> [<BOOL>] Output - <KEYSYM> ...
define fast_XptStackKeycodeKeysyms(display, keycode, /*[refresh]*/);
    lvars display, keycode, refresh = false;

    ;;; CHECK FOR OPTIONAL BOOLEAN ARGUMENT
    if isboolean(keycode) then
        (display, keycode) -> (display, keycode, refresh);
    endif;

    StackKeysymsForKeyCode(
        GetKeysymTableIndexForKeycode(display, keycode, refresh)
    );

enddefine;


;;; Returns the keycodes referred to by a given keysym - 03/12/91
;;; NOTE: Returned KeyCodeListPtr needs to be -XtFree-'s
;;; Input - <DISPLAYPTR> <KEYSYM> <KEYCODELISTPTR> <CARDINALPTR>
define fast_XtKeysymToKeycodeList() with_nargs 4;
    exacc (4) raw_XtKeysymToKeycodeList()
enddefine;


;;; Register a new case converter procedure - 13/01/92
;;; Input - <DISPLAYPTR> <XtCaseProc> <KEYSYM> <KEYSYM>
define fast_XtRegisterCaseConverter() with_nargs 4;
    exacc (4) raw_XtRegisterCaseConverter();
enddefine;


;;; Reinstall default case converter - 13/01/92
;;; Input - <DISPLAYPTR> <KEYSYM> <KEYSYM>
define fast_XptDefaultCaseConverter(display, start, fin);
    lvars display, start, fin;
    exacc(4) raw_XtRegisterCaseConverter(
        display,
        raw__XtConvertCase,
        start, fin
    );
enddefine;


;;; Register a new key translator - 13/01/92
;;; Input - <DISPLAYPTR> <XtKeyProc>
define fast_XtSetKeyTranslator() with_nargs 2;
    exacc (2) raw_XtSetKeyTranslator();
enddefine;


;;; Reinstall default key translator - 14/01/92
;;; Input - <DISPLAYPTR>
define fast_XptDefaultKeyTranslator(display);
    lvars display;
    exacc (2) raw_XtSetKeyTranslator(display, raw_XtTranslateKey)
enddefine;


;;; Default KeyCode to KeySym translator - 13/01/92
;;; Input - <DISPLAYPTR> <KEYCODE> <MODIFIERS> <MODIFIERSPTR> <KEYSYMPTR>
define fast_XtTranslateKey() with_nargs 5;
    exacc (5) raw_XtTranslateKey();
enddefine;


;;; "Pop" version of -fast_XtTranslateKey-
;;; Input - <DISPLAYPTR> <KEYCODE> <MODIFIERS>  Output - <KEYSYM> <MODIFIERS>
define fast_XptTranslateKey(display, keycode, mods);
    lvars display, keycode, mods;
    exacc (5) raw_XtTranslateKey(
        display, keycode, mods, XptModifiers_ptr, XptKeySym_ptr1
    );
    exacc :XptKeySym XptKeySym_ptr1;
    exacc :XptModifiers XptModifiers_ptr;
enddefine;


;;; Call current KeyCode to KeySym translator - 13/01/92
;;; Input - <DISPLAYPTR> <KEYCODE> <MODIFIERS> <MODIFIERSPTR> <KEYSYMPTR>
define fast_XtTranslateKeycode() with_nargs 5;
    exacc (5) raw_XtTranslateKeycode();
enddefine;


;;; "Pop" version of -fast_XtTranslateKeycode-
;;; Input - <DISPLAYPTR> <KEYCODE> <MODIFIERS>  Output - <KEYSYM> <MODIFIERS>
define fast_XptTranslateKeycode(display, keycode, mods);
    lvars display, keycode, mods;
    exacc (5) raw_XtTranslateKeycode(
        display, keycode, mods, XptModifiers_ptr, XptKeySym_ptr1
    );
    exacc :XptKeySym XptKeySym_ptr1;
    exacc :XptModifiers XptModifiers_ptr;
enddefine;


;;; Get the KeySym & modifiers from an action event
;;; Input - <XEVENTPTR> <FALSE or MODIFIERSPTR>  Output - <KEYSYM>
define fast_XtGetActionKeysym(ptr,mods) with_nargs 2;
    lvars ptr, mods, keysym;
    exacc (2):XptKeySym raw_XtGetActionKeysym(
        ptr,
        mods or null_external_ptr
    ) -> keysym;
    not(keysym == XNoSymbol) and keysym;
enddefine;


;;; "Pop" version of -fast_XtGetActionKeysym-
;;; Input - <XEVENTPTR>  Output - <KEYSYM> <MODIFIERS>
define fast_XptGetActionKeysym(eventptr);
    lvars eventptr, keysym;
    exacc (2):XptKeySym raw_XtGetActionKeysym(eventptr, XptModifiers_ptr) -> keysym;
    not(keysym == XNoSymbol) and keysym;
    exacc :XptModifiers XptModifiers_ptr;
enddefine;


;;;; so uses works ok
constant $-fast_xt_keyboard = true;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 30 1995
        Ensured that typespecs for arg structures passed to exfunc_exported
        procedures are correctly defined (i.e. every field an exptr/exval).
        Replaced all uses of exptr_in*it_fixed with EXPTRINITSTR
 */
