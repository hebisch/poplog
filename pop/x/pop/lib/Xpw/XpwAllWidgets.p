/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xpw/XpwAllWidgets.p
 > Purpose:         Load all Xpw widgets
 > Author:          John Gibson, Apr 25 1993
 > Documentation:   HELP * Xpw
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses
    XpwCore,
    XpwGraphic,
    XpwPixmap,
    XpwScrollText,
    xpwCompositeWidget,
    xpwTransparentWidget,
;

constant XpwAllWidgets = true;

endexload_batch;
endsection;
