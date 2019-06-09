/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/xmToggleButtonWidget.p
 > Purpose:         Motif widgetclass
 > Author:          John Gibson, Apr 14 1993
 > Documentation:   HELP * MOTIF
 > Related Files:   Xm/xm*Widget.p etc
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include xpt_coretypes.ph;

uses Xmgeneral, xmLabelWidget;

XptLoadWidgetClass xmToggleButtonWidget [^^XM_EXLIBS]
    xmToggleButtonWidget    <- xmToggleButtonWidgetClass,
    xmToggleButtonGadget    <- xmToggleButtonGadgetClass,
;

define XmIsToggleButton =
    XtIsSubclass(% xmToggleButtonWidget %);
enddefine;

define XmIsToggleButtonGadget =
    XtIsSubclass(% xmToggleButtonGadget %);
enddefine;

XptPopLoadProcedures ''
    XmToggleButtonGetState (x) :XptBoolean,
    XmToggleButtonSetState(x,y,z) :void,
    XmCreateToggleButton(w,x,y,z) :XptWidget,

    XmToggleButtonGadgetGetState (x) :XptBoolean,
    XmToggleButtonGadgetSetState(x,y,z) :void,
    XmCreateToggleButtonGadget(w,x,y,z) :XptWidget,
;

endexload_batch;
endsection;
