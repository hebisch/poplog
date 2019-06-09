/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xt_display.p
 > Purpose:         Dummy library, routines in binary
 > Author:          Adrian Howard, Aug  7 1990 (see revisions)
 > Documentation:   REF *XT_DISPLAY
 > Related Files:   C.x/x/pop/lib/xt_display.p
 */
compile_mode :pop11 +strict;

section;


/*
 * This is a dummy library that  can be loaded by the checking  version.
 * The routines that are expected to be included in this file are  built
 * into the Poplog binary. The  library has been  included so users  can
 * load it, protecting themselves against any movement of the procedures
 * from the core in future releases.
 *
 * The following identifiers are in the core system:
 *                      fast_XtDisplayInitialize,
 *                      fast_XtOpenDisplay,
 *                      fast_XtCloseDisplay,
 *                      fast_XtDisplayToApplicationContext,
 *                      XptImportDisplayPtr,
 *                      fast_XtDisplay,
 */


;;; So uses works OK
constant fast_xt_display = true;


endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun 25 1991 : removed reference to fast_xt_procs
--- Roger Evans, Dec  7 1990
        XptDisplayAppContext -> fast_XtDisplayToApplicationContext
--- Roger Evans, Nov 18 1990 tidied up
--- Adrian Howard, Sep  7 1990 : Added var so uses works
 */
