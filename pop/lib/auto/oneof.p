/*  --- Copyright University of Sussex 1996.  All rights reserved. ---------
 >  File:           C.all/lib/auto/oneof.p
 >  Purpose:        Return one element of a list at random
 >  Author:         S.Hardy 1978 (see revisions)
 >  Documentation:  REF * oneof
 >  Related Files:
 */
compile_mode:pop11 +strict;

section;

define oneof(list);
    lvars list, len = listlength(list);
    if len == 0 then
        mishap(list, 1, 'NON-EMPTY LIST NEEDED')
    else
        fast_subscrl(random(len), list)
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan  5 1996
        Improved
--- John Gibson, Oct 10 1992
        Cleaned up
 */
