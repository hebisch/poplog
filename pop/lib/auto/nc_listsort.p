/*  --- Copyright University of Sussex 1993. All rights reserved. ----------
 >  File:           C.all/lib/auto/nc_listsort.p
 >  Purpose:        Non-checking merge sort for lists
 >  Author:         Jonathan Cunningham, May 1981 (see revisions)
 >  Documentation:  HELP * SYSSORT
 >  Related Files:  LIB * SYSSORT, LIB * SORT
 */

compile_mode:pop11 +strict;

/*
The ordering predicate before_p should return true if its first argument
should precede its second and false if vice-versa. If it returns a non-
false value (e.g. 1) when its arguments are equal, then the relative
order of equal elements will be unchanged in the final list.

See * alphabefore for an example of such a predicate.
*/

section;

define nc_listsort(list, before_p);
    lvars       n, list;
    dlvars      l, x, i, procedure before_p;
    lconstant   merge_pair = writeable [[]];
    dlocal      % fast_back(merge_pair) %;

    define lconstant Get_run();
        ;;; Get a run of ordered elements from l
        l ->> x;
        fast_destpair(l) -> l -> i;
        repeat
            returnif(l == []);
            unless before_p(i, fast_front(l) ->> i) then
                [] -> fast_back(x);
                return
            endunless;
            fast_back(l ->> x) -> l
        endrepeat
    enddefine;

    define lconstant Merge(l1, l2);
        lvars l1, l2;
        merge_pair -> x;
        repeat
            if before_p(fast_front(l1),fast_front(l2)) then
                l1 ->> fast_back(x) -> x;
                if (fast_back(l1) ->> l1) == [] then
                    l2 -> fast_back(x);
                    return(fast_back(merge_pair))
                endif
            else
                l2 ->> fast_back(x) -> x;
                if (fast_back(l2) ->> l2) == [] then
                    l1 -> fast_back(x);
                    return(fast_back(merge_pair))
                endif
            endif
        endrepeat
    enddefine;

    define lconstant Get_sequence(n);
        lvars n;
        if n fi_< 2 then
            Get_run()
        else
            Get_sequence(n fi_- 1);
            returnif(l == []);
            Get_sequence(n fi_- 1);
            Merge()
        endif
    enddefine;

    returnif(list == []) ([]);

    back(list) ->;  /* Check (once!) that list is a pair */
    list -> l;
    Get_run();
    1 -> n;
    until l == [] do
        Get_sequence(n);
        Merge();
        n fi_+ 1 -> n
    enduntil
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jul 26 1993
        This file created from old LIB * SYSSORT
--- John Gibson, Aug 27 1990
        Changed note to weak
--- John Gibson, Jul 29 1990
        Tidied up, and in particular made sure that back of -merge_pair-
        is localised so it doesn't hold on to stuff afterwards.
--- John Gibson, Jun 18 1989
        Added -compile_mode-, and -writeable- for -merge_pair-
--- John Gibson, May  8 1988
        Lconstant'ed and lvars'ed
--- Mark Rubinstein, Feb 11 1986
        Fixed to work on dynamic lists.
*/
