/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/xmScaleWidget.p
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

XptLoadWidgetClass xmScaleWidget [^^XM_EXLIBS]
    xmScaleWidget   <- xmScaleWidgetClass,
;

define XmIsScale =
    XtIsSubclass(% xmScaleWidget %)
enddefine;

XptPopLoadProcedures ''
    XmScaleSetValue(x,y) :void,
    XmScaleGetValue(x,y) :void,
    XmCreateScale(w,x,y,z) :XptWidget,
;

endexload_batch;
endsection;
