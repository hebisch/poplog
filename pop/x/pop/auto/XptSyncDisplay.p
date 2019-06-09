/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptSyncDisplay.p
 > Purpose:         Sync a display
 > Author:          John Gibson, Mar 21 1994
 > Documentation:   REF * XptSyncDisplay
 */
compile_mode :pop11 +strict;

section;
exload_batch;

XptLoadProcedures XptSyncDisplay
lvars
    XSync(dpy,discard),
;

define XptSyncDisplay(dpy);
    lvars dpy, appcon = fast_XtDisplayToApplicationContext(
                                XptLiveTypeCheck(dpy, "DisplayPtr"));
    repeat
        exacc [fast] raw_XSync(dpy, false);
        quitunless(fast_XptAppTryEvents(appcon, true))
    endrepeat
enddefine;

endexload_batch;
endsection;
