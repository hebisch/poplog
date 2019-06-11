/*  --- Copyright University of Sussex 1990.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_plx.p
 >  Purpose:        exit from ved and call prolog on the current file.
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vars ved_plx;
    dlocal vedargument = 'x';
    ved_prolog();
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Simon Nichols, Nov  2 1990
        Changed to use new subsystem facilities.
 */
