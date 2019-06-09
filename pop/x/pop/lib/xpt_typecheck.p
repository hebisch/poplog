/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xpt_typecheck.p
 > Purpose:         Type checking routines for non-fast intrinsic routines
 > Author:          Adrian Howard, Jul 17 1990 (see revisions)
 > Documentation:   REF *XPT_TYPECHECK
 > Related Files:   C.x/x/pop/lib/xt_*, C.x/x/pop/include/xpt_constants.ph
 */

compile_mode:pop11 +strict;

section;


uses shadowkey_key;
include xt_constants;
include xpt_constants;
include xpt_generaltypes;

;;; AH - 10/09/91
;;; test for null terminated shadowclass vector structures
define lconstant check_nt_shadowlist(item, type);
    lvars item, type, len, n, test_item;
    XptLiveTypeCheck(item, type) ->;
    shadow_length(item) -> len;
    for n from len by -1 to 1 do;
        item(n) -> test_item;
        if test_item == false
        or (isexternal_ptr_class(test_item) and is_null_external_ptr(exacc :exptr test_item)
        )
        then
            return(item);
        endif;
    endfor;
    mishap(item, 1, 'NULL TERMINATED ' sys_>< type sys_>< ' NEEDED');
enddefine;

constant procedure XptCheckCardinal;

;;; AH - 10/09/91
;;; check a shadowclass vector has <= a certain number of elements
define lconstant check_shadowlist_and_length(item, len, type);
    lvars item, len, type;
    unless XptLiveTypeCheck(item, type)
    and XptCheckCardinal(len)
    and shadow_length(item) >= len
    do
        mishap(item, len, 2, type sys_>< ' SHORTER THAN GIVEN LENGTH');
    else
        return(item, len);
    endunless;
enddefine;

define XptCheckNTStringList = check_nt_shadowlist(%XDT_STRINGLIST%); enddefine;
define XptCheckNTCallbackList = check_nt_shadowlist(%XDT_CALLBACKLIST%); enddefine;
define XptCheckNTCacheRefList = check_nt_shadowlist(%XDT_CACHEREFLIST%); enddefine;

define XptCheckResourceListAndLength = check_shadowlist_and_length(%XDT_RESOURCELIST%); enddefine;
define XptCheckActionListAndLength = check_shadowlist_and_length(%XDT_ACTIONLIST%); enddefine;
define XptCheckStringListAndLength = check_shadowlist_and_length(%XDT_STRINGLIST%); enddefine;

define XptCheckOptionDescListAndLength(item, len) -> (item, len);
    if item or len /== 0 then
        check_shadowlist_and_length(XDT_OPTIONDESCLIST)
    endif
enddefine;

define XptCheckConvertArgListAndLength = check_shadowlist_and_length(%XDT_CONVERTARGLIST%); enddefine;
define XptCheckValueListAndLength = check_shadowlist_and_length(%XDT_VALUELIST%); enddefine;
define XptCheckWidgetListAndLength = check_shadowlist_and_length(%XDT_WIDGETLIST%); enddefine;


;;; Various checks for specific types
define XptCheckActionHookId = XptLiveTypeCheck(%XDT_ACTIONHOOKID%); enddefine;
define XptCheckActionList = XptLiveTypeCheck(%XDT_ACTIONLIST%); enddefine;
define XptCheckAppContext = XptLiveTypeCheck(%XDT_APPCONTEXT%); enddefine;
define XptCheckAppContextPtr = XptLiveTypeCheck(%XDT_APPCONTEXTPTR%); enddefine;
define XptCheckCacheRefList = XptLiveTypeCheck(%XDT_CACHEREFLIST%); enddefine;
define XptCheckCacheRefPtr = XptLiveTypeCheck(%XDT_CACHEREFPTR%); enddefine;
define XptCheckCallbackList = XptLiveTypeCheck(%XDT_CALLBACKLIST%); enddefine;
define XptCheckCardinalPtr = XptLiveTypeCheck(%XDT_CARDINALPTR%); enddefine;
define XptCheckConvertArgList = XptLiveTypeCheck(%XDT_CONVERTARGLIST%); enddefine;
define XptCheckGCValuesPtr = XptLiveTypeCheck(%XDT_GCVALUESPTR%); enddefine;
define XptCheckInputId = XptLiveTypeCheck(%XDT_INPUTID%); enddefine;
define XptCheckIntPtr = XptLiveTypeCheck(%XDT_INTPTR%); enddefine;
define XptCheckIntervalId = XptLiveTypeCheck(%XDT_INTERVALID%); enddefine;
define XptCheckKeyCodeListPtr = XptLiveTypeCheck(%XDT_KEYCODELISTPTR%); enddefine;
define XptCheckKeyCodePtr = XptLiveTypeCheck(%XDT_KEYCODEPTR%); enddefine;
define XptCheckKeySymPtr = XptLiveTypeCheck(%XDT_KEYSYMPTR%); enddefine;
define XptCheckKeySymTable = XptLiveTypeCheck(%XDT_KEYSYMTABLE%); enddefine;
define XptCheckModifiersPtr = XptLiveTypeCheck(%XDT_MODIFIERSPTR%); enddefine;
define XptCheckOptionDescList = XptLiveTypeCheck(%XDT_OPTIONDESCLIST%); enddefine;
define XptCheckPositionPtr = XptLiveTypeCheck(%XDT_POSITIONPTR%); enddefine;
define XptCheckResourceList = XptLiveTypeCheck(%XDT_RESOURCELIST%); enddefine;
define XptCheckResourceListPtr = XptLiveTypeCheck(%XDT_RESOURCELISTPTR%); enddefine;
define XptCheckScreenPtr = XptLiveTypeCheck(%XDT_SCREENPTR%); enddefine;
define XptCheckStringList = XptLiveTypeCheck(%XDT_STRINGLIST%); enddefine;
define XptCheckSubstitution = XptLiveTypeCheck(%XDT_SUBSTITUTION%); enddefine;
define XptCheckTimePtr = XptTypeCheck(%XDT_TIMEPTR%); enddefine;
define XptCheckValueList = XptLiveTypeCheck(%XDT_VALUELIST%); enddefine;
define XptCheckValuePtr = XptLiveTypeCheck(%XDT_VALUEPTR%); enddefine;
define XptCheckVarargsList = XptLiveTypeCheck(%XDT_VARARGSLIST%); enddefine;
define XptCheckWidgetList = XptLiveTypeCheck(%XDT_WIDGETLIST%); enddefine;
define XptCheckWindow = XptLiveTypeCheck(%XDT_WINDOW%); enddefine;
define XptCheckWorkProcId = XptLiveTypeCheck(%XDT_WORKPROCID%); enddefine;
define XptCheckXEventPtr = XptLiveTypeCheck(%XDT_XEVENTPTR%); enddefine;
define XptCheckXrmDatabase = XptLiveTypeCheck(%XDT_DATABASE%); enddefine;


;;; Check XptXrmValuePtr has valid address field
define XptCheckValidValuePtr() with_nargs 1;
    lvars value_ptr = XptCheckValuePtr();
    lvars addr = exacc :XptXrmValue value_ptr.XptXVAddr;

    if (isexternal_ptr(addr) and is_valid_external_ptr(addr))
    then
        value_ptr;
    else
        mishap(
            value_ptr, 1,
            'XptXrmValuePtr POINTS TO XptXrmValue WITH INVALID ADDRESS FIELD'
        );
    endif;
enddefine;


;;; Checks the argument is an XtGrabKind (int between 0 & 2) - 08/08/90
define XptCheckGrabKind(item);
    lvars item;
    if isinteger(item) and item fi_>= 0 and item fi_< 3 then
        return(item);
    else
        mishap(item, 1, 'GrabKind NEEDED');
    endif;
enddefine;


;;; Check XtListPosition (int between 0 & 1) - JM 4/7/91
define XptCheckListPosition(item);
    lvars item;
    if isinteger(item) and item fi_>= 0 and item fi_< 2 then
        return(item);
    else
        mishap(item, 1, 'ListPosition NEEDED');
    endif;
enddefine;



define XptCheckModifiers = XptCheckUnsignedInt(%%) enddefine;
define XptCheckCardinal = XptCheckUnsignedInt(%%) enddefine;
define XptCheckGCMask = XptCheckUnsignedInt(%%) enddefine;
define XptCheckCacheType = XptCheckUnsignedInt(%%) enddefine;

define XptCheckPosition = XptCheckShort(%%) enddefine;

define XptCheckCursor = XptCheckUnsignedIntegral(%%) enddefine;
define XptCheckGC = XptCheckUnsignedIntegral(%%) enddefine;
define XptCheckTime = XptCheckUnsignedIntegral(%%) enddefine;
define XptCheckInputMask = XptCheckUnsignedIntegral(%%) enddefine;
define XptCheckEventMask = XptCheckUnsignedIntegral(%%) enddefine;
define XptCheckKeySym = XptCheckUnsignedIntegral(%%) enddefine;


define XptCheckDevice(dev) -> dev;
    lvars dev;
    unless isdevice(dev) then
        mishap(dev,1,'DEVICE NEEDED');
    endunless;
enddefine;


;;; Checks argument is a procedure or false - 14/08/90
define XptCheckFilePredicate(item);
    lvars item;
    if item then
        XptCheckProcedure(item);
    else
        false;
    endif;
enddefine;


;;; Checks the argument is an XtKeyCode (int between 0 & 255) - 14/08/90
define XptCheckKeyCode(item);
    lvars item;
    if isinteger(item) and item fi_>= 0 and item fi_< 256 then
        return(item);
    else
        mishap(item, 1, 'KeyCode NEEDED');
    endif;
enddefine;


;;; So uses works OK
constant xpt_typecheck = true;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 16 1996
        Changed XptCheckOptionDescListAndLength to allow false (providing
        len is 0).
--- Adrian Howard, Jul  8 1993
        Added XptCheckValidValuePtr and XptCheckXrmDatabase
--- Adrian Howard, Jun 30 1993
        Added XptCheckScreenPtr
--- Adrian Howard, Jun 14 1993
        Made XptCheckPosition use XptCheckShort
--- John Gibson, May  5 1993
        Uses shadowkey_key (instead of old shadowkey)
--- John Gibson, Apr 17 1993
        Made XptTypeCheck, XptLiveTypeCheck, XptCheckWidget,
        XptCheckWidgetClass, XptCheckString, XptCheckShort,
        XptCheckUnsignedShort, XptCheckInt, XptCheckUnsignedInt,
        XptCheckUnsignedIntegral, XptCheckWindowedWidget autoloadable.
--- John Gibson, Apr 12 1993
        Made CHECKARGLIST macro autoloadable (For POPC, perm macros must be
        autoloadable separately from runtime code.)
--- Adrian Howard, May 28 1992
        Added -XptCheckWindowedWidget-
--- Adrian Howard, Mar  5 1992 : Fixed -XptCheckWindow-
--- Adrian Howard, Jan 13 1992 : Added -XptCheckModifiersPtr-
--- Adrian Howard, Dec  3 1991 : Added -XptCheckKeyCodeListPtr-
--- Adrian Howard, Dec  2 1991 : Added -XptCheckIntPtr- and -XptCheckKeyCodePtr-
--- Adrian Howard, Nov 29 1991
        o Fixed -XptCheckKeySymPtr-!
        o Added -XptCheckKeySym- & -XptCheckKeySymTable-
--- Adrian Howard, Nov 25 1991 : Added -XptCheckKeySymPtr-
--- Adrian Howard, Sep 11 1991 : Added more null-termination/length test for
        shadowclass list structures
--- John Gibson, Sep 11 1991
        Added declarations to make file compile ...
--- Adrian Howard, Sep 10 1991
        o Added tests for list structures of certain lengths
        o Added tests for null terminated list structures
--- Adrian Howard, Sep  5 1991 : Added -XptCheckValueList- and
        -XptCheckCacheRefPtr-
--- Jonathan Meyer, Jul  8 1991
        Made XptCheckTimePtr use XptTypeCheck rather than XptLiveTypeCheck
--- Adrian Howard, Jul  5 1991 : Removed -XptCheckBoolean-, -XptCoerceBoolean-
        should be used instead.
--- Jonathan Meyer, Jul  4 1991
        Added XptCheckListPosition, XptCheckEventMask, XptCheckBoolean
--- Roger Evans, Jul  2 1991 moved XptIsValidCallback to xpt_coretypes
--- Roger Evans, Jun 28 1991 fixed procedure checking routines for new
        callback stuff
--- Roger Evans, Jun  3 1991 added CHECKARGLIST
--- Jonathan Meyer, Feb 15 1991
        Moved code in XptTypeCheck into a separate library, xpt_typetest.
        Simplified XptTypeCheck and XptLiveTypeCheck corrospondingly.
--- Roger Evans, Feb  3 1991 improved checking in XptTypeCheck to maximise
        likelihood of sensible error messages!
--- Jonathan Meyer, Jan 30 1991 : added (unsigned) short check
--- Adrian Howard, Nov 26 1990 : Changed XptTypeCheck so it checks for an
        external pointer before it checks the XptDataType
--- Roger Evans, Nov 19 1990 removed XptCheckCacheRefPtr,XptCheckTranslations
--- Roger Evans, Nov 19 1990 changed XptCheckGCMask to be a uint check
--- Roger Evans, Nov 19 1990 added XptCheckDevice
--- Roger Evans, Nov 11 1990
    changed checking routines to use 'live' checker, got rid of
    external_ptr_field, improved and renamed XptCheckProcedure,
    moved back to main lib
--- Roger Evans, Oct 22 1990 moved to unsupp
--- Adrian Howard, Sep 18 1990 : Changed -field_spec_info- calls to SIZEOFTYPE
        macro
--- Adrian Howard, Sep 13 1990 : Made procedures global and integer checking
        system independant. Altered varargs checking so the proper checks
        were made on the special varargs resource names even if they are
        null-terminated
--- Adrian Howard, Sep  7 1990 : Altered bug where fast integer checking
        routines were used in places where non-simple ints occurred
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : Added check for CacheRefList structures
--- Adrian Howard, Sep  5 1990 : xt_constants --> xpt_constants & library
        changed from xt_typecheck to xpt_typecheck
--- Adrian Howard, Sep  4 1990 : StringNullVec --> StringList
 */
