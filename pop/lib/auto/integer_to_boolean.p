/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/integer_to_boolean.p
 > Purpose:         integer <-> boolean converter
 > Author:          John Gibson, May 18 1993
 > Documentation:   REF *EXTERNAL_DATA
 */
compile_mode :pop11 +strict :vm -pentch;

section;

define integer_to_boolean(/*int*/) with_nargs 1;
    if isinteger(dup()) then
        () /== 0
    else
        mishap((), 1, 'INTEGER NEEDED')
    endif
enddefine;
;;;
define updaterof integer_to_boolean(/*bool*/) with_nargs 1;
    if () then 1 else 0 endif
enddefine;

endsection;
