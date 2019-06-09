/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/auto/fast_XtIsWidget.p
 > Purpose:         Test whether a widget is a subclass of Core
 > Author:          Adrian Howard, May 28 1992
 > Documentation:   REF *XT_WIDGETCLASS
 > Related Files:   LIB *FAST_XT_WIDGETCLASS
 */


;;; This was seperated out from LIB *FAST_XT_WIDGETCLASS so it can be used
;;; in other libraries (eg LIB *XT_WIDGETINFO.) --- adrianh --- 28/05/92

compile_mode:pop11 +strict;
section;

include xpt_coretypes.ph;

XptLoadProcedures 'fast_XtIsWidget' lvars XtIsWidget;

;;; Returns -true- if widget is a subclass of Core - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define global fast_XtIsWidget() with_nargs 1;
    exacc (1):XptBoolean raw_XtIsWidget();
enddefine;

endsection;
