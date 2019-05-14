/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/auto/fast_ncrev.p
 >  Purpose:        fast non copying reverse a list
 >  Author:         Roger Evans, 1985 (see revisions)
 >  Documentation:  REF * FASTPROCS  HELP * EFFICIENCY
 >  Related Files:  LIB * NCREV
 */
compile_mode:pop11 +strict;

section;

define global fast_ncrev(l) -> p;
    lvars l, p = [];
    until l == [] do
        (fast_back(l), l, p) -> (l, p, fast_back(l))
    enduntil
enddefine;

endsection;
