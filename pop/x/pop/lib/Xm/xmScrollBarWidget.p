/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/xmScrollBarWidget.p
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

XptLoadWidgetClass xmScrollBarWidget [^^XM_EXLIBS]
    xmScrollBarWidget   <- xmScrollBarWidgetClass,
;

define XmIsScrollBar =
    XtIsSubclass(% xmScrollBarWidget %)
enddefine;

XptPopLoadProcedures ''
;;; _XmSetEtchedSlider(x) :void,
    XmCreateScrollBar(w,x,y,z) :XptWidget,
    XmScrollBarGetValues(v,w,x,y,z) :void,
    XmScrollBarSetValues(u,v,w,x,y,z) :void,
;

endexload_batch;
endsection;
