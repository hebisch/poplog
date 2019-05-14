/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/numerator.p
 > Purpose:         Return numerator of rational number
 > Author:          John Williams, Nov 20 1985 (see revisions)
 > Documentation:   REF * NUMBERS
 > Related Files:   LIB * DENOMINATOR
 */
compile_mode:pop11 +strict;

section;

sysunprotect("numerator");

define global numerator() with_nargs 1;
    destratio() ->
enddefine;

sysprotect("numerator");

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct  7 1992
        Added sysunprotect
 */
