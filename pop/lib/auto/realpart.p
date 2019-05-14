/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/auto/realpart.p
 >  Purpose:        Real part of a number
 >  Author:         John Gibson, Nov  8 1985 (see revisions)
 >  Documentation:  REF * NUMBERS
 >  Related Files:  LIB * IMAGPART
 */
compile_mode:pop11 +strict;

section;

sysunprotect("realpart");

define global realpart() with_nargs 1;
    destcomplex() ->
enddefine;

sysprotect("realpart");

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct  7 1992
        Added sysunprotect
 */
