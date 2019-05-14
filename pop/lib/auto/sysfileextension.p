/*  --- Copyright University of Sussex 1989. All rights reserved. ----------
 *  File:           C.unix/lib/auto/sysfileextension.p
 *  Purpose:        return filename extension *** UNIX version ***
 *  Author:         Mark Rubinstein, Jun 13 1985 (see revisions)
 *  Documentation:
 *  Related Files:  vms version in $usepop/master/C.vms/...
 */
compile_mode:pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

    ;;; same as -sys_fname_extn-
define global sysfileextension = sys_fname(%5%) enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug  4 1989
        Now same as -sys_fname_name-
--- John Gibson, Jul  1 1989
        Added +strict etc
 */
