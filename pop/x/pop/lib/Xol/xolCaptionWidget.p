/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/xolCaptionWidget.p
 > Purpose:         Define Xol widgetclass
 > Author:          John Gibson, Mar 31 1993
 > Documentation:   HELP * OPENLOOK
 > Related Files:   x/pop/lib/Xol/xol*Widget.p
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses Xolgeneral, XolManager;

XptLoadWidgetClass xolCaptionWidget [^^XOL_EXLIBS]
    xolCaptionWidget <- captionWidgetClass
;

endexload_batch;
endsection;
