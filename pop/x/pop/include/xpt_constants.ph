/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/include/xpt_constants.ph
 > Purpose:         Various Poplog X constants
 > Author:          Adrian Howard, Aug 20 1990 (see revisions)
 > Documentation:   REF *XPT_CONSTANTS
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p
 */

#_TERMIN_IF DEF XPT_CONSTANTS_INCLUDED

section;

;;; NB: this file should be regarded as read-only: many of the values
;;;     defined here are also compiled into the poplog image itself, so
;;;     changing them here will only lead to inconsistency


iconstant

    ;;; data type names
    XDT_ACTIONHOOKID    = "ActionHookId",
    XDT_ACTIONLIST      = "ActionList",
    XDT_APPCONTEXT      = "AppContext",
    XDT_APPCONTEXTPTR   = "AppContextPtr",
    XDT_ARGLIST         = "ArgList",
    XDT_CACHEREF        = "CacheRef",
    XDT_CACHEREFLIST    = "CacheRefList",
    XDT_CACHEREFPTR     = "CacheRefPtr",
    XDT_CACHETYPE       = "CacheType",
    XDT_CALLBACKLIST    = "CallbackList",
    XDT_CARDINALPTR     = "CardinalPtr",
    XDT_CONVERTARGLIST  = "ConvertArgList",
    XDT_DATABASE     = "Database",
    XDT_DISPLAYPTR      = "DisplayPtr",
    XDT_GCMASK          = "GCMask",
    XDT_GCVALUESPTR     = "GCValuesPtr",
    XDT_INPUTID         = "InputId",
    XDT_INTERVALID      = "IntervalId",
    XDT_INTPTR          = "IntPtr",
    XDT_KEYCODELISTPTR  = "KeyCodeListPtr",
    XDT_KEYCODEPTR      = "KeyCodePtr",
    XDT_KEYSYMPTR       = "KeySymPtr",
    XDT_KEYSYMTABLE     = "KeySymTable",
    XDT_MODIFIERSPTR    = "ModifiersPtr",
    XDT_OPTIONDESCLIST  = "OptionDescList",
    XDT_POINTER         = "Pointer",
    XDT_POSITIONPTR     = "PositionPtr",
    XDT_PROCEDURE       = "Procedure",
    XDT_RESOURCELIST    = "ResourceList",
    XDT_RESOURCELISTPTR = "ResourceListPtr",
    XDT_SCREENPTR       = "ScreenPtr",
    XDT_STRINGLIST      = "StringList",
    XDT_SUBSTITUTION    = "Substitution",
    XDT_TIMEPTR         = "TimePtr",
    XDT_TRANSLATIONS    = "Translations",
    XDT_VALUELIST       = "ValueList",
    XDT_VALUEPTR        = "ValuePtr",
    XDT_VARARGSLIST     = "VarArgsList",
    XDT_WIDGET          = "Widget",
    XDT_WIDGETCLASS     = "WidgetClass",
    XDT_WIDGETLIST      = "WidgetList",
    XDT_WINDOW          = "Window",
    XDT_WORKPROCID      = "WorkProcId",
    XDT_XEVENTPTR       = "XEventPtr",
;

iconstant macro (
    ;;; default classname for Poplog applications
    XT_POPLOG_CLASSNAME = 'Poplog\(0)',

);

iconstant XPT_CONSTANTS_INCLUDED = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jul  8 1993
        Changed XDT_XRMDATABASE to XDT_DATABASE to fit in with XDT_VALUEPTR
--- Adrian Howard, Jun 30 1993
        Added XDT_XRMDATABASE
--- Adrian Howard, Jan 13 1992 : Added -XDT_MODIFIERSPTR-
--- Adrian Howard, Dec  3 1991 : Added -XDT_KEYCODELISTPTR-
--- Adrian Howard, Dec  2 1991 : Added -XDT_KEYCODEPTR- & -XDT_INTPTR-
--- Adrian Howard, Nov 29 1991 : Added -XDT_KEYSYMTABLE-
--- Adrian Howard, Nov 25 1991 : Added -XDT_KEYSYMPTR-
--- Adrian Howard, Oct 31 1991 : Added -XDT_CACHEREF-
--- Adrian Howard, Sep  5 1991
        o Added -XDT_VALUELIST- & -XDT_CACHEREFPTR-
        o Removed obsolete callback type constants
--- Jonathan Meyer, Feb  9 1991
        Made all XDT_ defs non-macros, because words in macro versions break
        libraries declaring shadowclasses.
--- John Gibson, Feb  9 1991
        Made all defs macros (better, 'cos they work in more contexts)
--- Roger Evans, Oct 30 1990 added XptCBTypeActionHook
--- Roger Evans, Oct 17 1990 added XPT_CONSTANTS_INCLUDED
--- James Goodlet, Oct 15 1990 - added -XDT_WINDOW-
--- Roger Evans, Oct 11 1990 Much revised
    Added XT_POPLOG_CLASSNAME
    Added XDT_PROCEDURE
--- Adrian Howard, Sep 20 1990 : Corrected typo
--- Adrian Howard, Sep 19 1990 : Added -XDT_POINTER-
--- Adrian Howard, Sep 14 1990 : Added -XDT_SCREENPTR-
--- Adrian Howard, Sep 13 1990 : Fixed and null-terminated string-constants
        to save on garbage when calling Xt routines
--- Ian Rogers, Sep  5 1990 - fixed typos
--- Adrian Howard, Sep  5 1990 : Added XDT_CACHEREFLIST
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck & name of
        library changed from xt_constants to xpt_constants.
--- Adrian Howard, Sep  4 1990 : StringNullVec --> StringList
 */
