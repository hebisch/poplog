/*  --- Copyright University of Sussex 1994. All rights reserved. ----------
 >  File:           C.unix/lib/auto/syskill.p
 >  Purpose:        Moved out of the system.  Kill a process.
 >  Author:         John Gibson, Aug 1 1984 (see revisions)
 >  Documentation:
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;
include sigdefs.ph;

define syskill = sys_send_signal(% SIG_KILL %) enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec  8 1994
        Tidied up
 */
