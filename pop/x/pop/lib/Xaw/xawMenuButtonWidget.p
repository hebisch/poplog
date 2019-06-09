/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xaw/xawMenuButtonWidget.p
 > Purpose:         Athena widgetclass and associated procedures
 > Author:          John Gibson, Apr 17 1993
 > Documentation:   REF * ATHENA
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses xawCommandWidget;

XptLoadWidgetClass xawMenuButtonWidget [^^XAW_EXLIBS]
    xawMenuButtonWidget <- menuButtonWidgetClass,
;

define XawIsMenuButtonWidget =
    XtIsSubclass(% xawMenuButtonWidget %)
enddefine;

endexload_batch;
endsection;
