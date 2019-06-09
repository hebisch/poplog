/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_composite.p
 > Purpose:         Checking routines to manage children in composite widgets
 > Author:          Adrian Howard, Jul 17 1990 (see revisions)
 > Documentation:   REF *XT_COMPOSITE
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p,
 >                  C.x/x/pop/lib/fast_xt_composite.p
 */

compile_mode:pop11 +strict;

section;

uses xpt_typecheck.p;        ;;; Get the type checking routines
uses fast_xt_composite.p;   ;;; Get the fast versions of the procedures

include xpt_generaltypes.ph;


;;; Add a widget to its parents list of managed children - 17/07/90
;;; Input - <Widget>
define global XtManageChild() with_nargs 1;
    fast_XtManageChild(XptCheckWidget());
enddefine;


;;; Remove a widget from its parents list of managed children - 17/07/90
;;; Input - <Widget>
define global XtUnmanageChild() with_nargs 1;
    fast_XtUnmanageChild(XptCheckWidget());
enddefine;


;;; Returns -true- if the widget is managed - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define global XtIsManaged() with_nargs 1;
    fast_XtIsManaged(XptCheckWidget());
enddefine;


;;; Alter value of a widgets "map_when_managed" field - 17/07/90
;;; Input - <Widget> <BOOL>
define global XtSetMappedWhenManaged(widget, new_value);
    lvars widget, new_value;
    fast_XtSetMappedWhenManaged(XptCheckWidget(widget), new_value);
enddefine;


;;; Add a list of widgets to their parents list(s) of managed children
;;; - 27/07/90
;;; Input - <WidgetList> <Cardinal>
define global XtManageChildren(widgetlist, cardinal);
    lvars widgetlist, cardinal;
    fast_XtManageChildren(XptCheckWidgetListAndLength(widgetlist, cardinal));
enddefine;


;;; Remove a list of widgets to their parents list(s) of managed children
;;; - 27/07/90
;;; Input - <WidgetList> <Cardinal>
define global XtUnmanageChildren(widgetlist, cardinal);
    lvars widgetlist, cardinal;
    fast_XtUnmanageChildren(XptCheckWidgetListAndLength(widgetlist, cardinal));
enddefine;


;;; So uses works OK
constant xt_composite= true;


endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Sep 11 1991 : Added tests for widget list length
--- Roger Evans, Nov 18 1990 tidied up
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck
 */
