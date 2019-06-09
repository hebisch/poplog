/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_init.p
 > Purpose:         Various initialization routines
 > Author:          Adrian Howard, Aug  7 1990 (see revisions)
 > Documentation:   REF *XT_INIT
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p,
 >                  C.x/x/pop/lib/fast_xt_init.p
 */
compile_mode:pop11 +strict;

section;
exload_batch;

uses fast_xt_init.p;        ;;; Get the fast versions of the procedures

/*
 * This library contains dummy procedures that are built into the Poplog binary
 * The library should be loaded so users are protected against any movement of
 * the procedures from  the core in future releases.
 *
 * The following identifiers are in the core system:
 *      XptDefaultSetup
 */

define XptToolkitPreInitialize;
    fast_XptToolkitPreInitialize();
enddefine;

define XptToolkitPostInitialize;
    fast_XptToolkitPostInitialize();
enddefine;

;;; Initialize the X Toolkit Internals (only done once) - 07/08/90
define XtToolkitInitialize;
    fast_XtToolkitInitialize();
enddefine;


;;; Initialize a widget class without creating any widgets - 07/08/90
;;; Input - <WidgetClass>
define XtInitializeWidgetClass() with_nargs 1;
    fast_XtInitializeWidgetClass(XptCheckWidgetClass());
enddefine;


;;; Do custom initialisation - 21/06/91
define XptCustomInitialize() with_nargs 1;
    fast_XptCustomInitialize(XptCheckProcedure());
enddefine;


;;; So uses works OK
constant xt_init = true;

endexload_batch;
endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jul  5 1991 : Added references to -XptDefaultSetup-
--- Adrian Howard, Jun 21 1991 : Added XptCustomInitialize
--- Roger Evans, Nov 19 1990 add XptInitialize routines
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : Minor buggette corrected
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck
 */
