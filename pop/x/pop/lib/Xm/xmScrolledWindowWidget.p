/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/xmScrolledWindowWidget.p
 > Purpose:         Motif widgetclass
 > Author:          John Gibson, Apr 14 1993
 > Documentation:   HELP * MOTIF
 > Related Files:   Xm/xm*Widget.p etc
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include xpt_coretypes.ph;

uses Xmgeneral;

XptLoadWidgetClass xmScrolledWindowWidget [^^XM_EXLIBS]
    xmScrolledWindowWidget  <- xmScrolledWindowWidgetClass,
;

define XmIsScrolledWindow =
    XtIsSubclass(% xmScrolledWindowWidget %)
enddefine;

XptPopLoadProcedures ''
;;; InitializeScrollBars(x) :void,
    XmScrolledWindowSetAreas(w,x,y,z) :void,
    XmCreateScrolledWindow(w,x,y,z) :XptWidget,
;

endexload_batch;
endsection;
