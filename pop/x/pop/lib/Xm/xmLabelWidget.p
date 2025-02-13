/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/xmLabelWidget.p
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

XptLoadWidgetClass xmLabelWidget [^^XM_EXLIBS]
    xmLabelWidget   <- xmLabelWidgetClass,
    xmLabelGadget   <- xmLabelGadgetClass,
;

define XmIsLabel =
    XtIsSubclass(% xmLabelWidget %);
enddefine;

define XmIsLabelGadget =
    XtIsSubclass(% xmLabelGadget %);
enddefine;

XptPopLoadProcedures ''
    XmCreateLabel(w,x,y,z) :XptWidget,
    XmCreateLabelGadget(w,x,y,z) :XptWidget,
;

endexload_batch;
endsection;
