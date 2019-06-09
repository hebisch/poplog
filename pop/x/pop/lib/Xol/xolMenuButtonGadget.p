/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/xolMenuButtonGadget.p
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

XptLoadWidgetClass xolMenuButtonGadget [^^XOL_EXLIBS]
    xolMenuButtonGadget <- menuButtonGadgetClass
;

XptSpecifyResourceInfo(xolMenuButtonGadget,
    [[[menuPane Widget]] ^false]
);

endexload_batch;
endsection;
