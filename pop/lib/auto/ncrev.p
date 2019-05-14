/*  --- Copyright University of Sussex 1989. All rights reserved. ----------
 >  File:           C.all/lib/auto/ncrev.p
 >  Purpose:        non-copying list reversal
 >  Author:         Jonathan Cunningham, Jun 20 1983 (see revisions)
 >  Documentation:  HELP * NCREV
 >  Related Files:  LIB * FAST_NCREV
 */
compile_mode:pop11 +strict;

section;

define global ncrev(l) -> p;
    lvars l, p = [];
    until null(l) do
        (fast_back(l), l, p) -> (l, p, fast_back(l))
    enduntil
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 30 1989
        Added +strict
--- Aaron Sloman, Jul 24 1988
    Made to work faster - nearly four times faster
--- Mark Rubinstein, Feb 11 1986 - made to work on dynamic lists.
*/
