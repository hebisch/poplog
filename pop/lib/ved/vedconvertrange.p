/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/vedconvertrange.p
 > Purpose:         Character conversion within marked range
 > Author:          John Gibson, Feb 15 1992
 > Documentation:   REF *VEDPROCS
 */
compile_mode :pop11 +strict;

section;

define global vedconvertrange(test_p, convert_p);
    lvars oldchanged = vedchanged, procedure (test_p, convert_p);
    vedpositionpush();
    vedmarkfind();
    vedconvertline(test_p, convert_p, vvedmarkhi-vvedmarklo+1);
    vedpositionpop();
    if oldchanged then oldchanged + 1 else 1 endif -> vedchanged
enddefine;

endsection;
