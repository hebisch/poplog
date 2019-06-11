/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_ccw.p
 > Purpose:         Change case of characters in the current word(s)
 > Author:          Rob Duncan, Oct 10 1989
 > Documentation:   REF * VEDPROCS
 > Related Files:   LIB * VED_UCW, * VED_LCW
 */
compile_mode :pop11 +strict;

section;

define lconstant changecase(c);
    lvars c;
    if islowercode(c) then
        lowertoupper(c);
    else
        uppertolower(c);
    endif;
enddefine;

define ved_ccw;
    vedconvertword(isalphacode, changecase, vedargument, true);
enddefine;

endsection;
