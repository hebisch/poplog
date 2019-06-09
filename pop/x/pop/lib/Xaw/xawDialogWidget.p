/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xaw/xawDialogWidget.p
 > Purpose:         Athena widgetclass and associated procedures
 > Author:          John Gibson, Apr 17 1993
 > Documentation:   REF * ATHENA
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses xawFormWidget;

XptLoadWidgetClass xawDialogWidget [^^XAW_EXLIBS]
    xawDialogWidget <- dialogWidgetClass,
;

define XawIsDialogWidget =
    XtIsSubclass(% xawDialogWidget %)
enddefine;

XptPopLoadProcedures ''
    XawDialogAddButton(w,x,y,z) :void,
    XawDialogGetValueString(x) :exptr.exacc_ntstring,
;

endexload_batch;
endsection;
