/*  --- Copyright University of Sussex 1990.  All rights reserved. ---------
 >  File:           C.all/lib/auto/shuffle.p
 >  Purpose:        Randomly reorder a list
 >  Author:         John Gibson, Jan  5 1996
 >  Documentation:  REF * shuffle
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define shuffle(list);
    lvars n = 0, r, item, list;
    for item in list do
        n fi_+ 1 -> n;
        random(n) ->> r;
        subscr_stack(r) -> subscr_stack(1);
        item -> subscr_stack(r)
    endfor;
    conslist(n)
enddefine;

endsection;
