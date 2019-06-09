/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_widgetinfo.p
 > Purpose:         Checking routines that give info on the status of a widget
 > Author:          Adrian Howard, Jul 17 1990 (see revisions)
 > Documentation:   REF *XT_WIDGETINFO
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p,
 >                  C.x/x/pop/lib/fast_xt_widgetinfo.p
 */
compile_mode:pop11 +strict;

section;
exload_batch;

uses fast_xt_widgetinfo.p;  ;;; Get the fast versions of the procedures


;;; Returns -true- if widget has been realized - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define XtIsRealized() with_nargs 1;
    fast_XtIsRealized(XptCheckWidget());
enddefine;


;;; Returns the window for the specified widget - 17/07/90
;;; Input - <Widget>, Output - <ScreenPtr>
define XtScreen() with_nargs 1;
    fast_XtScreen(XptCheckWindowedWidget());
enddefine;


;;; Returns the display pointer for the object if it's a widget, otherwise the
;;; display pointer of its nearest widget ancestor - 17/07/90
;;; Input - <Widget>, Output - <DisplayPtr>
define XtDisplayOfObject() with_nargs 1;
    fast_XtDisplayOfObject(XptCheckWidget());
enddefine;


;;; Returns the window for the object if it's a widget, otherwise the
;;; window of its nearest widget ancestor - 17/07/90
;;; Input - <Widget>, Output - <ScreenPtr>
define XtScreenOfObject() with_nargs 1;
    fast_XtScreenOfObject(XptCheckWidget());
enddefine;


;;; Return the window of the specified widget - 27/07/90
;;; Input - <Widget>, Output - <Window>
define XtWindow() with_nargs 1;
    fast_XtWindow(XptCheckWindowedWidget());
enddefine;


;;; Returns the window for the object if it's a widget, otherwise the
;;; window of its nearest widget ancestor - 27/07/90
;;; Input - <Widget>, Output - <Window>
define XtWindowOfObject() with_nargs 1;
    fast_XtWindowOfObject(XptCheckWidget());
enddefine;


;;; Returns a pointer to the instance name of an object - 31/08/90
;;; Input - <Widget>, Output - <String>
define XtName() with_nargs 1;
    fast_XtName(XptCheckWidget());
enddefine;


;;; Returns the parent of a specified widget - 31/08/90
;;; Input - <Widget>, Output - <Widget | FALSE>
define XtParent() with_nargs 1;
    fast_XtParent(XptCheckWidget());
enddefine;


;;; So uses works OK
constant xt_widgetinfo= true;

endexload_batch;
endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, May 28 1992
        Added use of -XptCheckWindowedWidget- in appropriate places
--- Roger Evans, Nov 19 1990 tidied up
--- Adrian Howard, Sep 13 1990 : Moved -XtDisplay- to LIB *XT_DISPLAY
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck
--- Adrian Howard, Aug 31 1990 : Added XtName & XtParent on RobDu's request
 */
