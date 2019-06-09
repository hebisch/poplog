/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/xmSeparatorWidget.p
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

XptLoadWidgetClass xmSeparatorWidget [^^XM_EXLIBS]
    xmSeparatorWidget   <- xmSeparatorWidgetClass,
    xmSeparatorGadget   <- xmSeparatorGadgetClass,
;

define XmIsSeparator =
    XtIsSubclass(% xmSeparatorWidget %)
enddefine;

define XmIsSeparatorGadget =
    XtIsSubclass(% xmSeparatorGadget %)
enddefine;

XptPopLoadProcedures ''
    XmCreateSeparator(w,x,y,z) :XptWidget,
    XmCreateSeparatorGadget(w,x,y,z) :XptWidget,
;

endexload_batch;
endsection;
