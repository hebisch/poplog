/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 >  File:           C.all/lib/auto/phase.p
 >  Purpose:        Get complex phase of a number.
 >  Author:         John Gibson, Nov  8 1985 (see revisions)
 >  Documentation:  REF * NUMBERS
 */
compile_mode :pop11 +strict;

section;

define phase() with_nargs 1;
    arctan2(destcomplex())
enddefine;

sysprotect("phase");

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 19 1995
        Tidied
 */
