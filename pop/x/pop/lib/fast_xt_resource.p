/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xt_resource.p
 > Purpose:         Resource management
 > Author:          Adrian Howard, Aug  9 1990 (see revisions)
 > Documentation:   REF *XT_RESOURCE
 > Related Files:   C.x/x/pop/lib/xt_resource.p
 */

compile_mode: pop11 +strict;
section;

/*
 * The following routines are  built into the Poplog binary. The library
 * has been included so users can load it, protecting themselves against
 * any movement of the procedures from the core in future releases.
 *
 * The following identifiers are in the core system:
 *                     fast_XtGetResourceList
 *                     fast_XtGetConstraintResourceList
 *                     fast_XtGetApplicationResources
 *                     fast_XtVaGetApplicationResources
 *                     fast_XtGetValues
 *                     fast_XtVaGetValues
 *                     fast_XtGetSubvalues
 *                     fast_XtVaGetSubvalues
 *                     fast_XtSetValues
 *                     fast_XtVaSetValues
 *                     fast_XtSetSubvalues
 *                     fast_XtVaSetSubvalues
 */

include xpt_xtypes;

XptLoadProcedures fast_xt_resource lvars XtDatabase;

define fast_XtDatabase() with_nargs 1;
    exacc (1):XptXrmDatabase raw_XtDatabase();
enddefine;

;;; So uses works OK
constant fast_xt_resource= true;


endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun 30 1993
        Added fast_XtDatabase
--- Adrian Howard, Jun 25 1991 : Removed reference to fast_xt_procs
--- Roger Evans, Nov 19 1990 tidied up
--- Adrian Howard, Sep  7 1990 : Added var so uses works
 */
