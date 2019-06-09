/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xpw/xpwScrollTextWidget.p
 > Purpose:         Xpw widgetclass
 > Author:          John Gibson, Apr  5 1993
 > Documentation:   REF * XpwScrollText
 > Related Files:   Xpw/xpw*Widget.p
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses XpwCallMethod;

XptLoadWidgetClass xpwScrollTextWidget [^^XPW_EXLIBS]
    xpwScrollTextWidget <- xpwScrollTextWidgetClass
;

endexload_batch;
endsection;
