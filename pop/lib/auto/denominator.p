/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/denominator.p
 > Purpose:         Return denominator of rational number
 > Author:          John Williams, Nov 20 1985 (see revisions)
 > Documentation:   REF * NUMBERS
 > Related Files:   LIB * NUMERATOR
 */
compile_mode:pop11 +strict;

section;

sysunprotect("denominator");

define global denominator() -> d with_nargs 1;
    destratio() -> (, d)
enddefine;

sysprotect("denominator");

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct  7 1992
        Added sysunprotect
 */
