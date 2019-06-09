/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xaw/xawPanedWidget.p
 > Purpose:         Athena widgetclass and associated procedures
 > Author:          John Gibson, Apr 17 1993
 > Documentation:   REF * ATHENA
 */
compile_mode :pop11 +strict;

section;
exload_batch;

XptLoadWidgetClass xawPanedWidget [^^XAW_EXLIBS]
    xawPanedWidget  <- panedWidgetClass,
;

define XawIsPanedWidget =
    XtIsSubclass(% xawPanedWidget %)
enddefine;

XptPopLoadProcedures ''
    XawPanedSetMinMax(x,y,z) :void,
    XawPanedGetMinMax(x,y,z) :void,
    XawPanedSetRefigureMode(x,y) :void,
    XawPanedGetNumSub(x) :int,
    XawPanedAllowResize(x,y) :void,
;

endexload_batch;
endsection;
