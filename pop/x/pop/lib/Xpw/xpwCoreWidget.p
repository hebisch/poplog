/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xpw/xpwCoreWidget.p
 > Purpose:         Xpw widgetclass
 > Author:          John Gibson, Apr  5 1993
 > Documentation:   REF * XpwCore
 > Related Files:   Xpw/xpw*Widget.p
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses XpwCallMethod;

XptLoadWidgetClass xpwCoreWidget [^^XPW_EXLIBS]
    xpwCoreWidget <- xpwCoreWidgetClass
;

endexload_batch;
endsection;
