/*  --- Copyright University of Sussex 1991.  All rights reserved. ---------
 >  File:           C.all/lib/auto/delete.p
 >  Purpose:        delete objects from list
 >  Author:         Unknown (see revisions)
 >  Documentation:  REF * LISTS
 >  Related Files:  LIB * NCDELETE
 */
compile_mode :pop11 +strict;

section;

define global delete(item, list) -> list;
    lvars item, list, trail_len = 0, n, equal_p;
    if isinteger(list) then
        ;;; delete n occurences
        ((), item, list) -> (item, list, n);
        fi_check(n, 0, false) ->
    else
        ;;; delete all
        -1 -> n
    endif;
    if isprocedure(list) then
        ;;; equals procedure to use
        ((), item, list) -> (item, list, equal_p)
    else
        nonop = -> equal_p
    endif;

    #|  repeat
            if n == 0 then
                list;
                1 -> trail_len;
                quitloop
            elseif null(list) then
                quitloop
            elseif fast_apply(fast_front(list), item, equal_p) then
                n fi_- 1 -> n;
                0 -> trail_len
            else
                list;
                trail_len fi_+ 1 -> trail_len
            endif;
            fast_back(list) -> list
        endrepeat
    |# -> n;        ;;; new length
    erasenum([], trail_len) -> list;

    fast_repeat n fi_- trail_len times
        conspair(fast_front(), list) -> list
    endrepeat
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 12 1991
        Rewritten to use user stack rather than recursion, and to share
        trailing sublist with original where possible. Also allowed optional
        equals procedure to use.
--- Aaron Sloman, Nov  6 1986  lvarsed, and optimised.
*/
