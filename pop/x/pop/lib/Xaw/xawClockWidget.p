/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xaw/xawClockWidget.p
 > Purpose:         Athena widgetclass and associated procedures
 > Author:          John Gibson, Apr 17 1993
 > Documentation:   REF * ATHENA
 */
compile_mode :pop11 +strict;

section;

XptLoadWidgetClass xawClockWidget [^^XAW_EXLIBS]
    xawClockWidget  <- clockWidgetClass,
;

define XawIsClockWidget =
    XtIsSubclass(% xawClockWidget %)
enddefine;

endsection;
