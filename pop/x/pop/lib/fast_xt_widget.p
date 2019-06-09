/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xt_widget.p
 > Purpose:         Dummy library, routines in binary
 > Author:          Adrian Howard, Aug  8 1990 (see revisions)
 > Documentation:   REF *XT_WIDGET
 > Related Files:   C.x/x/pop/lib/xt_widget.p
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
 * The following identifers are in the core system:
 *                      fast_XtCreateWidget,
 *                      fast_XtVaCreateWidget,
 *                      fast_XtAppCreateShell,
 *                      fast_XtVaAppCreateShell,
 *                      fast_XtRealizeWidget,
 *                      fast_XtUnrealizeWidget,
 *                      fast_XtDestroyWidget,
 *                      fast_XtCreateManagedWidget,
 *                      fast_XtVaCreateManagedWidget,
 *                      fast_XtMapWidget,
 *                      fast_XtUnmapWidget,
 *                      fast_XptImportWidget,
 */


;;; So uses works OK
constant fast_xt_widget= true;


endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun 25 1991 : Removed reference to fast_xt_procs
--- Roger Evans, Nov 19 1990 tidied up
--- Adrian Howard, Sep  7 1990 : Added var so uses works
 */
