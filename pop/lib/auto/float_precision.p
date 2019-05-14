/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/auto/float_precision.p
 >  Purpose:        Return the number of significant digits in a float
 >  Author:         John Gibson, Jan  2 1986 (see revisions)
 >  Documentation:  REF *NUMBERS
 >  Related Files:
 */
compile_mode:pop11 +strict;

section;

sysunprotect("float_precision");

define global float_precision(float) -> n;
    lvars float, n;
    float_digits(float) -> n;
    if float = 0.0 then 0 -> n endif;
enddefine;

sysprotect("float_precision");

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Oct 10 1992
        Cleaned up
 */
