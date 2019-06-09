/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/xmMainWindowWidget.p
 > Purpose:         Motif widgetclass
 > Author:          John Gibson, Apr 14 1993
 > Documentation:   HELP * MOTIF
 > Related Files:   Xm/xm*Widget.p etc
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include xpt_coretypes.ph;

uses Xmgeneral, xmScrolledWindowWidget;

XptLoadWidgetClass xmMainWindowWidget [^^XM_EXLIBS]
    xmMainWindowWidget  <- xmMainWindowWidgetClass,
;

define XmIsMainWindow =
    XtIsSubclass(% xmMainWindowWidget %)
enddefine;

XptPopLoadProcedures ''
    XmMainWindowSetAreas(u,v,w,x,y,z) :void,
    XmMainWindowSep1(x) :XptWidget,
    XmMainWindowSep2(x) :XptWidget,
    XmMainWindowSep3(x) :XptWidget,
    XmCreateMainWindow(w,x,y,z) :XptWidget,
;

endexload_batch;
endsection;
