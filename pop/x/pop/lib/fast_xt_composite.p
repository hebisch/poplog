/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xt_composite.p
 > Purpose:         Fast routines to manage children in composite widgets
 > Author:          Adrian Howard, Jul 17 1990 (see revisions)
 > Documentation:   REF *XT_COMPOSITE
 > Related Files:   C.x/x/pop/lib/xt_composite.p
 */

compile_mode:pop11 +strict;

section;

include xpt_coretypes.ph;

;;; Load the external "raw" procedures
XptLoadProcedures fast_xt_composite
    lvars
        XtManageChildren
        XtManageChild
        XtUnmanageChildren
        XtUnmanageChild
        XtIsManaged
        XtSetMappedWhenManaged
;

;;; Add a widget to its parents list of managed children - 17/07/90
;;; Input - <Widget>
define global fast_XtManageChild() with_nargs 1;
    exacc (1) raw_XtManageChild();
enddefine;

;;; Remove a widget from its parents list of managed children - 17/07/90
;;; Input - <Widget>
define global fast_XtUnmanageChild() with_nargs 1;
    exacc (1) raw_XtUnmanageChild();
enddefine;

;;; Returns -true- if the widget is managed - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define global fast_XtIsManaged() with_nargs 1;
    exacc (1):XptBoolean raw_XtIsManaged();
enddefine;

;;; Alter value of a widgets "map_when_managed" field - 17/07/90
;;; Input - <Widget> <BOOL>
define global fast_XtSetMappedWhenManaged() with_nargs 2;
    exacc (2) raw_XtSetMappedWhenManaged(updater(XptCoerceBoolean)());
enddefine;

;;; Add a list of widgets to their parents list(s) of managed children
;;; - 27/07/90
;;; Input - <WidgetList> <Cardinal>
define global fast_XtManageChildren() with_nargs 2;
    exacc (2) raw_XtManageChildren();
enddefine;

;;; Remove a list of widgets from their parents list(s) of managed children
;;; - 27/07/90
;;; Input - <WidgetList> <Cardinal>
define global fast_XtUnmanageChildren() with_nargs 2;
    exacc (2) raw_XtUnmanageChildren();
enddefine;

constant fast_xt_composite= true;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  3 1991
        includes xpt_coretypes.ph
--- Adrian Howard, Jun 25 1991 : Removed reference to fast_xt_procs
--- Roger Evans, Nov 18 1990 tidied up
--- Roger Evans, Oct 19 1990 changed to use XptLoadProcedures
--- Roger Evans, Oct 16 1990 fixed updater typo
--- Roger Evans, Oct 11 1990 changed to use exacc
--- James Goodlet, Sep 12 1990 - made definitions global.
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  6 1990 : Altered calls to XptCoerce/Import to make
        compatable with format of new external conversion/access procs
--- Adrian Howard, Sep  5 1990 : Bug fix on fast_XtSetMappedWhenManaged. Added
        a #_IF DEF hack to make library work while RogerE's old xt_composite
        library is still around.
 */
