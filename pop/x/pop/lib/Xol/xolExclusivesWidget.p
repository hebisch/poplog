/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/xolExclusivesWidget.p
 > Purpose:         Define Xol widgetclass
 > Author:          John Gibson, Mar 31 1993
 > Documentation:   HELP * OPENLOOK
 > Related Files:   x/pop/lib/Xol/xol*Widget.p
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses Xolgeneral, XolManager;

XptLoadWidgetClass xolExclusivesWidget [^^XOL_EXLIBS]
    xolExclusivesWidget <- exclusivesWidgetClass
;

endexload_batch;
endsection;
