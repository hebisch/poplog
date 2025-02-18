/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xaw/xawGripWidget.p
 > Purpose:         Athena widgetclass and associated procedures
 > Author:          John Gibson, Apr 17 1993
 > Documentation:   REF * ATHENA
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses xawSimpleWidget;

XptLoadWidgetClass xawGripWidget [^^XAW_EXLIBS]
    xawGripWidget   <- gripWidgetClass,
;

define XawIsGripWidget =
    XtIsSubclass(% xawGripWidget %)
enddefine;

endexload_batch;
endsection;
