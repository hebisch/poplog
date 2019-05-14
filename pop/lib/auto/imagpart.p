/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/auto/imagpart.p
 >  Purpose:        Imaginary part of a number
 >  Author:         John Gibson, Nov  8 1985 (see revisions)
 >  Documentation:  REF * NUMBERS
 >  Related Files:  LIB * REALPART
 */
compile_mode:pop11 +strict;

section;

sysunprotect("imagpart");

define global imagpart() -> i with_nargs 1;
    lvars (, i) = destcomplex();
enddefine;

sysprotect("imagpart");

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct  7 1992
        Added sysunprotect
 */
