/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/auto/sort.p
 >  Purpose:        SORT uses merge sort procedure defined in SYSSORT.P
 >  Author:         A.Sloman 1982 (see revisions)
 >  Documentation:  HELP * SORT
 >  Related Files:
 */
compile_mode:pop11 +strict;


section;

define global sort(list);
lvars list;
    if null(list) then
        []
    elseif isnumber(fast_front(list)) then
        syssort(list, nonop <=)
    else
        syssort(list, alphabefore)
    endif
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 10 1992
        Added strict, changed empty test to null, and changed order procedure
        to <= rather than < (quicker, since preserves existing ordering
        among equals).
 */
