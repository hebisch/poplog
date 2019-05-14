/* --- Copyright University of Sussex 1987.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/auto/fi_mod.p
 > Purpose:        Fast integer modulo
 > Author:         Aaron Sloman, Mar  1 1987
 > Documentation:  REF * FASTPROCS
 > Related Files:  fi_div, fi_rem
 */

section;

define global constant 2 x fi_mod y;
    lvars x y;
    x fi_// y -> -> x;
    if x fi_< 0 then
        if y fi_>= 0 then y fi_+ x else x endif
    elseif y fi_< 0 then
        y fi_+ x
    else
        x
    endif
enddefine;

endsection;
