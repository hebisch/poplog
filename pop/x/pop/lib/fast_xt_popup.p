/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xt_popup.p
 > Purpose:         Dummy library, routines in binary
 > Author:          Adrian Howard, Aug  8 1990 (see revisions)
 > Documentation:   REF *XT_POPUP
 > Related Files:   C.x/x/pop/lib/xt_popup.p
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
 *                      fast_XtCreatePopupShell,
 *                      fast_XtVaCreatePopupShell,
 *                      fast_XtPopup,
 *                      fast_XtPopupSpringLoaded,
 *                      fast_XtCallbackNone,
 *                      fast_XtCallbackNonexclusive,
 *                      fast_XtCallbackExclusive,
 *                      fast_XtCallbackPopdown,
 *                      fast_XtPopdown,
 *                      raw_XtCallbackNone,
 *                      raw_XtCallbackNonexclusive,
 *                      raw_XtCallbackExclusive,
 *                      raw_XtCallbackPopdown,
 */


;;; So uses works OK
constant fast_xt_popup= true;


endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun 25 1991 : Removed reference to fast_xt_procs
--- Roger Evans, Nov 19 1990 tidied up
--- Adrian Howard, Sep  7 1990 : Added var so uses works
 */
