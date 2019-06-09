/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/auto/fast_XptWidgetOfObject.p
 > Purpose:         Returns the widget whose window a given gadget uses
 > Author:          Adrian Howard, Sep 23 1992
 > Documentation:   REF *fast_XptWidgetInfo
 > Related Files:   XptWidgetOfObject.p
 */


uses fast_xt_widgetinfo;
uses fast_xt_util;
uses fast_xt_widgetclass;

compile_mode: pop11 +strict;
section;

define global fast_XptWidgetOfObject(w) -> w;
    lvars w;
    if fast_XtIsRealized(w) then
        ;;; IF THE OBJECT IS REALIZED THEN WE CAN GET AT ITS WIDGET VIA
        ;;; ITS WINDOW
        fast_XtWindowToWidget(
            fast_XtDisplayOfObject(w),
            fast_XtWindowOfObject(w)
        ) -> w;
    else
        ;;; OTHERWISE, WE HAVE TO CLIMB THE WIDGET TREE
        until fast_XtIsWidget(w) then
            fast_XtParent(w) -> w;
        enduntil;
    endif;
enddefine;

endsection;
