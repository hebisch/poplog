/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/xmCascadeButtonWidget.p
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

XptLoadWidgetClass xmCascadeButtonWidget [^^XM_EXLIBS]
    xmCascadeButtonWidget   <- xmCascadeButtonWidgetClass,
    xmCascadeButtonGadget   <- xmCascadeButtonGadgetClass,
;

define XmIsCascadeButton =
    XtIsSubclass(% xmCascadeButtonWidget %);
enddefine;

define XmIsCascadeButtonGadget =
    XtIsSubclass(% xmCascadeButtonGadget %);
enddefine;

XptPopLoadProcedures ''
    XmCreateCascadeButton(w,x,y,z) :XptWidget,
    XmCascadeButtonHighlight(x,y) :void,
    XmCreateCascadeButtonGadget(w,x,y,z) :XptWidget,
    XmCascadeButtonGadgetHighlight(x,y) :void,
;

endexload_batch;
endsection;
