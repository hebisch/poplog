/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/list_util.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *LISTS
 */

;;; -------------- MISCELLANEOUS LIST PROCEDURES -------------------------

#_INCLUDE 'declare.ph'

;;; --------------------------------------------------------------------

define atom() with_nargs 1;
    not(ispair())
enddefine;

define pdtolist(p);
    lvars p;
    Sys$-Check_procedure(p);
    Sys$-Ensure_writeable(conspair(true, p))
enddefine;

define fast_ncdelete(item, list) -> list;
    lvars item, next, list, temp, n, procedure eq_p;
    if isinteger(list) then
        ;;; delete n occurrences
        ((), item, list) -> (item, list, n)
    else
        ;;; delete all
        -1 -> n
    endif;
    if isprocedure(list) then
        ;;; equals procedure to use
        ((), item, list) -> (item, list, eq_p)
    else
        nonop = -> eq_p
    endif;

    repeat
        returnif(n == 0 or list == []);
        quitunless(eq_p(fast_front(list), item));
        fast_back(list) -> list;
        n fi_- 1 -> n
    endrepeat;

    list -> temp;
    until (fast_back(temp) ->> next) == [] do
        if eq_p(fast_front(next), item) then
            fast_back(next) -> fast_back(temp);
            quitif((n fi_- 1 ->> n) == 0)
        else
            next -> temp
        endif
    enduntil
enddefine;

define copylist(list) -> newlist;
    lvars list, last, newlist, list2;
    if null(list) then
        [] -> newlist
    else
        conspair(fast_front(list), []) ->> last -> newlist;
        fast_back(list) -> list;
        until null(list) do
            conspair(fast_front(list), []) ->> fast_back(last) -> last;
            fast_back(list) -> list
        enduntil
    endif
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb  2 1995
        Changed pdtolist to use Ensure_writeable rather than writeable (latter
        sets writeable_ignored true)
--- John Williams, Jun 29 1993
        Reversed order of arguments to eq_p in fast_ncdelete (cf. BR ianr.42)
--- John Gibson, Oct 12 1991
        Made -fast_ncdelete- take optional eq_p and number-to-delete args
--- John Gibson, Apr  9 1988
        Bits of old lists.p, listutil.p
 */
