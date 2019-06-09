/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xaw/xawViewportWidget.p
 > Purpose:         Athena widgetclass and associated procedures
 > Author:          John Gibson, Apr 17 1993
 > Documentation:   REF * ATHENA
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses xawFormWidget;

XptLoadWidgetClass xawViewportWidget [^^XAW_EXLIBS]
    xawViewportWidget   <- viewportWidgetClass,
;

define XawIsViewportWidget =
    XtIsSubclass(% xawViewportWidget %)
enddefine;

endexload_batch;
endsection;
