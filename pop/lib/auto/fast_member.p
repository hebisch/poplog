/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 *  File:           $usepop/master/C.all/lib/auto/fast_member.p
 *  Purpose:        fast member, tests with = but doesn't work on dynamic lists
 *  Author:         Aaron Sloman, 1985
 *  Documentation:  REF * FASTPROCS   HELP * EFFICIENCY
 *  Related Files:  LIB * SLOWPROCS
 */

section;

define global constant procedure fast_member(item,list);
lvars item,list;
    until list == [] do
        if (fast_destpair(list) -> list) = item then
            return(true);
        endif
    enduntil;
    false
enddefine;

endsection;
