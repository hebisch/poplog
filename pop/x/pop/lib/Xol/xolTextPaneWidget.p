/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/xolTextPaneWidget.p
 > Purpose:         Define Xol widgetclass
 > Author:          John Gibson, Mar 31 1993
 > Documentation:   HELP * OPENLOOK
 > Related Files:   x/pop/lib/Xol/xol*Widget.p
 */
compile_mode :pop11 +strict;

#_TERMIN_IF XOL_VERSION > 3000

section;
exload_batch;

uses Xolgeneral;

XptLoadWidgetClass xolTextPaneWidget [^^XOL_EXLIBS]
    xolTextPaneWidget <- textPaneWidgetClass
;

endexload_batch
endsection;
