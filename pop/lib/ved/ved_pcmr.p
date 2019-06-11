/*  --- Copyright University of Sussex 1990. All rights reserved. ----------
 >  File:           C.all/lib/ved/ved_pcmr.p
 >  Purpose:        prolog-compile marked range
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vars ved_pcmr();
    dlocal vedargument = 'lmr';
    ved_prolog();
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Simon Nichols, Nov  2 1990
        Changed to use new subsystem facilities.
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
