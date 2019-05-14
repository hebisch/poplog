/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/auto/float_digits.p
 >  Purpose:        Return the number of representation digits in a float
 >  Author:         John Gibson, Jan  2 1986 (see revisions)
 >  Documentation:  REF *NUMBERS
 */
compile_mode:pop11 +strict;

section;

sysunprotect("float_digits");

define global float_digits(float);
    lvars float;
    if isdecimal(float) then
        if issimple(float) then
            fast_subscrv(2, pop_float_parameters)   ;;; decimal params
        else
            fast_subscrv(3, pop_float_parameters)   ;;; ddecimal params
        endif -> float;
        fast_subscrv(1, float)          ;;; number of digits in the format
    else
        mishap(float, 1, 'DECIMAL OR DDECIMAL NEEDED')
    endif
enddefine;

sysprotect("float_digits");

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Oct 10 1992
        Cleaned up
 */
