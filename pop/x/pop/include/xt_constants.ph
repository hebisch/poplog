/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/include/xt_constants.ph
 > Purpose:         Various Xtoolkit constants
 > Author:          Roger Evans, Oct 10 1990 (see revisions)
 > Documentation:   REF *XT_CONSTANTS
 > Related Files:
 */

#_TERMIN_IF DEF XT_CONSTANTS_INCLUDED

include sysdefs.ph;

section;

;;; NB: this file should be regarded as read-only: it is included in the
;;;     poplog image itself, so changing values here will only lead to
;;;     inconsistency

iconstant macro (

    ;;; XrmOptionKind enum type
    XrmoptionNoArg      = 0,
    XrmoptionIsArg      = 1,
    XrmoptionStickyArg  = 2,
    XrmoptionSepArg     = 3,
    XrmoptionResArg     = 4,
    XrmoptionSkipArg    = 5,
    XrmoptionSkipLine   = 6,
    XrmoptionSkipNArgs  = 7,
);

iconstant macro (

    ;;; Special varargs resource names
    XtVaNested          = copy_fixed('XtVaNested'),
    XtVaTypedArg        = copy_fixed('XtVaTypedArg'),

);


iconstant macro (
    ;;; XtAddressMode enum type
    XtAddress           = 0,
    XtBaseOffset        = 1,
    XtImmediate         = 2,
    XtResourceString    = 3,
    XtResourceQuark     = 4,
    XtWidgetBaseOffset  = 5,
    XtProcedureArg      = 6,

    ;;; XtGrabKind enum type
    XtGrabNone          = 0,
    XtGrabNonexclusive  = 1,
    XtGrabExclusive     = 2,

    ;;; Condition mask values for XtAppAddInput
    XtInputNoneMask     = 0,
    XtInputReadMask     = 1,
    XtInputWriteMask    = 2,
    XtInputExceptMask   = 4,

    ;;; Event mask values for XtAppPending, XtAppProcessEvent etc.
    XtIMXEvent          = 1,
    XtIMTimer           = 2,
    XtIMAlternateInput  = 4,
    XtIMAll             = (XtIMXEvent || XtIMTimer || XtIMAlternateInput),

    ;;; Event handler insertion positions,
    XtListHead          = 0,
    XtListTail          = 1,
    XtAllEvents         = -1,

    ;;; Resource converter Cache modes (XtSetTypeConverter, etc.)
    XtCacheNone         = 1,
    XtCacheAll          = 2,
    XtCacheByDisplay    = 3,
    XtCacheRefCount     = 256,

    ;;; Callback list status (XtHasCallbacks)
    XtCallbackNoList    = 0,
    XtCallbackHasNone   = 1,
    XtCallbackHasSome   = 2,

);

iconstant XT_CONSTANTS_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 29 1995
        Values for XrmoptionSkipLine and XrmoptionSkipNArgs were swapped --
        corrected. Changed XtAllEvents to be -1.
--- John Gibson, Nov 21 1991
        Removed VMS #_IF.
--- Adrian Howard, Aug 29 1991 : Added -XtCallback*- constants
--- Jason Handby, Aug 27 1991 : Added XtCache*
--- Adrian Howard, Jul  5 1991 : Added reference to REF *XT_CONSTANTS
--- Jonathan Meyer, Jul  4 1991
        Added XtListHead/XtListTail
--- John Gibson, Feb  9 1991
        Made all defs macros (better, 'cos they work in more contexts)
--- John Gibson, Feb  2 1991
        Corrected VMS condition mask values (same as Unix after all)
--- John Gibson, Jan 24 1991
        Added VMS modifications
--- Roger Evans, Oct 17 1990 added XT_CONSTANTS_INCLUDED
--- Roger Evans, Oct 11 1990 moved out of xpt_constants.ph
 */
