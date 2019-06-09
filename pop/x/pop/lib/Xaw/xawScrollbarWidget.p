/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xaw/xawScrollbarWidget.p
 > Purpose:         Athena widgetclass and associated procedures
 > Author:          John Gibson, Apr 17 1993
 > Documentation:   REF * ATHENA
 */
compile_mode :pop11 +strict;

section;
exload_batch;

XptLoadWidgetClass xawScrollbarWidget [^^XAW_EXLIBS]
    xawScrollbarWidget  <- scrollbarWidgetClass,
;

define XawIsScrollbarWidget =
    XtIsSubclass(% xawScrollbarWidget %)
enddefine;

XptPopLoadProcedures ''
    XawScrollbarSetThumb(x,y,z) :void,
;

endexload_batch;
endsection;
