/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xaw/xawSimpleWidget.p
 > Purpose:         Athena widgetclass and associated procedures
 > Author:          John Gibson, Apr 17 1993
 > Documentation:   REF * ATHENA
 */
compile_mode :pop11 +strict;

section;

XptLoadWidgetClass xawSimpleWidget [^^XAW_EXLIBS]
    xawSimpleWidget <- simpleWidgetClass,
;

define XawIsSimpleWidget =
    XtIsSubclass(% xawSimpleWidget %)
enddefine;

endsection;
