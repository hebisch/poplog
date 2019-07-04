/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-guiXlibs.p
 > Purpose:         Some Xlib routines required by the Poplog UI
 > Author:          Julian Clinton, June 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section $-poplog_ui;

XptLoadProcedures guiXlibs
    lvars
    /* Xlib procedures */
    XRaiseWindow XFlush XSetInputFocus
;

define XRaiseWindow(display, widget);
lvars display widget;
    exacc (2) raw_XRaiseWindow(display, widget);
enddefine;

define XFlush(display);
lvars display;
    exacc (1) raw_XFlush(display);
enddefine;

define XSetInputFocus(display, focus, revert_to, time);
lvars display focus revert_to time;
    exacc (4) raw_XSetInputFocus(display, focus, revert_to, time);
enddefine;

constant guiXlibs = true;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 28 1993
        Changes for POPC
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Removed XSync.
    Removed popmemlim increment (now done in pop_ui_popcontroltool.p).
Julian Clinton, 7/8/91
    Added XSync.
Julian Clinton, 18/6/91
    Added wrappers.
    Added XSetInputFocus.
 */
