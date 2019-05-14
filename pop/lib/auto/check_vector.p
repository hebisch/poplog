/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/check_vector.p
 > Purpose:         Test for a string
 > Author:          John Gibson, Oct 16 1992
 > Documentation:   REF *VECTORS
 */
compile_mode :pop11 +strict :vm -pentch;

section;

define check_vector(item) with_props false;
    lvars item;
    unless isvector(item) then
        mishap(item, 1, 'VECTOR NEEDED')
    endunless
enddefine;

endsection;
