/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/xmArrowButtonWidget.p
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

XptLoadWidgetClass xmArrowButtonWidget [^^XM_EXLIBS]
    xmArrowButtonWidget <- xmArrowButtonWidgetClass,
    xmArrowButtonGadget <- xmArrowButtonGadgetClass,
;

define XmIsArrowButton =
    XtIsSubclass(% xmArrowButtonWidget %);
enddefine;

define XmIsArrowButtonGadget =
    XtIsSubclass(% xmArrowButtonGadget %);
enddefine;

XptPopLoadProcedures ''
    XmCreateArrowButton(w,x,y,z) :XptWidget,
    XmCreateArrowButtonGadget(w,x,y,z) :XptWidget,
;

endexload_batch;
endsection;
