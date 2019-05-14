/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/auto/pop_optimise.p
 > Purpose:         Old variable replaced by not(pop_debugging)
 > Author:          John Gibson, Jun  1 1989
 > Documentation:   REF *VMCODE
 */

section;

define global active pop_optimise;
    not(pop_debugging)
enddefine;

define updaterof active pop_optimise(/*bool*/) with_nargs 1;
    if (/*bool*/) then
        false -> pop_debugging
    elseunless pop_debugging then
        "undef" -> pop_debugging
    endif
enddefine;

endsection;
