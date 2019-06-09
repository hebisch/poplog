/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/xmPushButtonWidget.p
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

XptLoadWidgetClass xmPushButtonWidget [^^XM_EXLIBS]
    xmPushButtonWidget  <- xmPushButtonWidgetClass,
    xmPushButtonGadget  <- xmPushButtonGadgetClass,
;

define XmIsPushButton =
    XtIsSubclass(% xmPushButtonWidget %);
enddefine;

define XmIsPushButtonGadget =
    XtIsSubclass(% xmPushButtonGadget %);
enddefine;

XptPopLoadProcedures ''
    XmCreatePushButton(w,x,y,z) :XptWidget,
    XmCreatePushButtonGadget(w,x,y,z) :XptWidget,
;

endexload_batch;
endsection;
