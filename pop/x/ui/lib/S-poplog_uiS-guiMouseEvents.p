/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-guiMouseEvents.p
 > Purpose:         Mouse event handling
 > Author:          Julian Clinton, August 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section $-poplog_ui;

exload_batch;

uses
    xt_init,
    xt_grab,
    xt_trans,
;

define is_doubleclick(display, last_time) -> this_time -> boole;
lvars display, last_time, boole, this_time, timeout;
    exacc ^int (XtLastTimestampProcessed(display)) -> this_time;
    XtGetMultiClickTime(display) -> timeout;
    if this_time - last_time < timeout then
        true
    else
        false
    endif -> boole;
enddefine;

constant guiMouseEvents = true;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 28 1993
        Changes for POPC
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Removed popmemlim increment (now done in pop_ui_popcontroltool.p).
 */
