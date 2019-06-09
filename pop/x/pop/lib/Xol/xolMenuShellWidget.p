/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/xolMenuShellWidget.p
 > Purpose:         Define Xol widgetclass
 > Author:          John Gibson, Mar 31 1993
 > Documentation:   HELP * OPENLOOK
 > Related Files:   x/pop/lib/Xol/xol*Widget.p
 */
compile_mode :pop11 +strict;

uses-now XptSpecifyResourceInfo;

section;
exload_batch;

uses Xolgeneral, XolMenu;

XptLoadWidgetClass xolMenuShellWidget [^^XOL_EXLIBS]
    xolMenuShellWidget <- menuShellWidgetClass
;

XptSpecifyResourceInfo(xolMenuShellWidget,
    [[[menuPane Widget]] ^false]
);

endexload_batch;
endsection;
