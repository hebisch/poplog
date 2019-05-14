/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/auto/lastpair.p
 >  Purpose:        Access/update last pair in list (don't expand dynamic list)
 >  Author:         John Williams, Jul 1985 (see revisions)
 >  Documentation:  HELP *LASTPAIR
 >  Related Files:  LAST ISDYNAMIC
 */
compile_mode :pop11 +strict;

section;

define lastpair(l) -> l;
    lvars m;
    back(l) -> m;
    while ispair(m) do
        fast_back(m ->> l) -> m
    endwhile
enddefine;

define updaterof lastpair(x, l);
    lvars m;
    back(l) -> m;
    while ispair(back(m)) do
        fast_back(m ->> l) -> m
    endwhile;
    x -> fast_back(l)
enddefine;

endsection;
