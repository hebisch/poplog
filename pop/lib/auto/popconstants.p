/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/auto/popconstants.p
 > Purpose:         Old variable replaced by pop_debugging == true
 > Author:          John Gibson, Jun  1 1989
 > Documentation:   REF *IDENT
 */

section;

define global active popconstants;
    pop_debugging /== true
enddefine;

define updaterof active popconstants(/*bool*/) with_nargs 1;
    if (/*bool*/) then
        if pop_debugging == true then
            "undef" -> pop_debugging
        endif
    else
        true -> pop_debugging
    endif
enddefine;

endsection;
