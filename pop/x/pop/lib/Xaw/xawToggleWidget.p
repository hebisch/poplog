/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xaw/xawToggleWidget.p
 > Purpose:         Athena widgetclass and associated procedures
 > Author:          John Gibson, Apr 17 1993
 > Documentation:   REF * ATHENA
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses xawCommandWidget;

XptLoadWidgetClass xawToggleWidget [^^XAW_EXLIBS]
    xawToggleWidget <- toggleWidgetClass,
;

define XawIsToggleWidget =
    XtIsSubclass(% xawToggleWidget %)
enddefine;

XptPopLoadProcedures ''
    XawToggleChangeRadioGroup(x,y) :void,
    XawToggleGetCurrent(x) :exptr.exacc_ntstring,
    XawToggleSetCurrent(x,y) :void,
    XawToggleUnsetCurrent(x) :void,
;

endexload_batch;
endsection;
