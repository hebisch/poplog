/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xt_init.p
 > Purpose:         Dummy library, routines in binary
 > Author:          Adrian Howard, Aug  7 1990 (see revisions)
 > Documentation:   REF *XT_INIT
 > Related Files:   C.x/x/pop/lib/xt_init.p
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
 *                      fast_XtToolkitInitialize
 *                      fast_XtInitializeWidgetClass
 *                      fast_XptToolkitPreInitialize
 *                      fast_XptToolkitPostInitialize
 *                      XptDefaultAppContext,
 *                      XptDefaultDisplay,
 */


;;; So uses works OK
constant fast_xt_init= true;


endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jul  5 1991 : Moved references to -XptDefaultSetup- and
        -XptSetArgv- to LIB *XT_INIT
--- Adrian Howard, Jun 25 1991 : Removed reference to fast_xt_procs
--- Adrian Howard, Jun 21 1991 : Added XptSetArgv to list of procs
--- Roger Evans, Nov 19 1990 tidied up
--- Adrian Howard, Sep  7 1990 : Added var so uses works
 */
