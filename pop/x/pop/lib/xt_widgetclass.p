/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_widgetclass.p
 > Purpose:         Various checking routines related to the class and type
 >                  of widgets
 > Author:          Adrian Howard, Jul 17 1990 (see revisions)
 > Documentation:   REF *XT_WIDGETCLASS
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p,
 >                  C.x/x/pop/lib/fast_xt_widgetclass.p
 */

compile_mode:pop11 +strict;

section;
exload_batch;

uses fast_xt_widgetclass; ;;; Get the fast versions of the procedures


;;; Returns the class of a widget - 17/07/90
;;; Input - <Widget>, Output - <WidgetClass>
define XtClass() with_nargs 1;
    fast_XtClass(XptCheckWidget());
enddefine;


;;; Returns the superclass of a widget - 17/07/90
;;; Input - <Widget>, Output - <WidgetClass>
define XtSuperclass() with_nargs 1;
    fast_XtSuperclass(XptCheckWidget());
enddefine;


;;; Returns -true- if widget is a subclass of the Object class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define XtIsObject() with_nargs 1;
    fast_XtIsObject(XptCheckWidget());
enddefine;


;;; Returns -true- if widget is a subclass of the RectObj class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define XtIsRectObj() with_nargs 1;
    fast_XtIsRectObj(XptCheckWidget());
enddefine;


;;; Returns -true- if widget is a subclass of the Widget class! - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define XtIsWidget() with_nargs 1;
    fast_XtIsWidget(XptCheckWidget());
enddefine;


;;; Returns -true- if widget is a subclass of the Composite class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define XtIsComposite() with_nargs 1;
    fast_XtIsComposite(XptCheckWidget());
enddefine;


;;; Returns -true- if widget is a subclass of the Constraint class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define XtIsConstraint() with_nargs 1;
    fast_XtIsConstraint(XptCheckWidget());
enddefine;


;;; Returns -true- if widget is a subclass of the Shell class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define XtIsShell() with_nargs 1;
    fast_XtIsShell(XptCheckWidget());
enddefine;


;;; Returns -true- if widget is a subclass of OverrideShell class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define XtIsOverrideShell() with_nargs 1;
    fast_XtIsOverrideShell(XptCheckWidget());
enddefine;


;;; Returns -true- if widget is a subclass of WMShell class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define XtIsWMShell() with_nargs 1;
    fast_XtIsWMShell(XptCheckWidget());
enddefine;


;;; Returns -true- if widget is a subclass of TransientShell class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define XtIsTransientShell() with_nargs 1;
    fast_XtIsTransientShell(XptCheckWidget());
enddefine;


;;; Returns -true- if widget is a subclass of TopLevelShell class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define XtIsTopLevelShell() with_nargs 1;
    fast_XtIsTopLevelShell(XptCheckWidget());
enddefine;


;;; Returns -true- if widget a subclass of ApplicationShell class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define XtIsApplicationShell() with_nargs 1;
    fast_XtIsApplicationShell(XptCheckWidget());
enddefine;


;;; Returns -true- if widget a subclass of VendorShell class - 09/08/90
;;; Input - <Widget>, Output - <BOOL>
define XtIsVendorShell() with_nargs 1;
    fast_XtIsVendorShell(XptCheckWidget());
enddefine;


;;; So uses works OK
constant xt_widgetclass= true;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 14 1993
        Made XtIsSubclass autoloadable
--- Roger Evans, Nov 19 1990 tidied up
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck
 */
