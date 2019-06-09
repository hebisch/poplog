/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xpw/xpwTransparentWidget.p
 > Purpose:         Xpw widgetclass
 > Author:          John Gibson, Apr  5 1993
 > Documentation:   REF * XpwTransparent
 > Related Files:   Xpw/xpw*Widget.p
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses XpwCallMethod;

XptLoadWidgetClass xpwTransparentWidget [^^XPW_EXLIBS]
    xpwTransparentWidget <- xpwTransparentWidgetClass
;

endexload_batch;
endsection;
