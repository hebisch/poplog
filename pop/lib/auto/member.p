/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/member.p
 > Purpose:         Variable list-member procedure
 > Author:          John Gibson, Dec  4 1995
 > Documentation:   REF * LISTS
 */
compile_mode :pop11 +strict;

section;

define vars member(/*item, list*/) with_nargs 2;
    lmember_=() and true
enddefine;

endsection;
