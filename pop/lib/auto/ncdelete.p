/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:           C.all/lib/auto/ncdelete.p
 > Purpose:        non copying removal of matching items from a list
 > Author:         Aaron Sloman, Dec 24 1976 (see revisions)
 > Documentation:  REF * LISTS
 > Related Files:  LIB * DELETE, SRC * LIST_UTIL.P
 */
compile_mode:pop11 +strict;


;;; remove from the list, every item which is equivalent to the given
;;; item according the the equivalence test supplied.
;;; Do not copy the list: i.e. destructive (non-constructive??).

section;

define global ncdelete(item, list) -> list;
    lvars la, item, list, n, eq_p;
    if isinteger(list) then
        ;;; delete n occurrences
        ((), item, list) -> (item, list, n);
        fi_check(n, 0, false) ->
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

    ;;; skip leading matches of -item-
    repeat
        returnif(n == 0 or null(list));
        quitunless(fast_apply(fast_front(list), item, eq_p));
        fast_back(list) -> list;
        n fi_- 1 -> n
    endrepeat;

    ;;; rebuild tail of list, omitting -item-s
    list -> la;
    until null(fast_back(la)) do
        ;;; here, there must be (at least) two elements in list "la"
        ;;; we know the first element /== item, so test second
        if fast_apply(fast_front(fast_back(la)), item, eq_p) then
            ;;; unlink the pair containing item
            fast_back(fast_back(la)) -> fast_back(la);
            quitif((n fi_- 1 ->> n) == 0)
        else
            ;;; skip this element
            fast_back(la) -> la
        endif
    enduntil
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Williams, Jun 29 1993
        Reversed order of arguments to eq_p (cf. BR ianr.42)
--- John Gibson, Oct 12 1991
        Rewritten to take same arguments as -delete-, i.e. optional eq_p
        and optional number to delete.
--- John Gibson, Jul  3 1989
        Added +strict
--- Aled Morris, Nov 22 1987
        Optimised with careful use of -null- with fast_ front and back.
        Added comments.
--- Mark Rubinstein, Feb 11 1986 - made to work on dynamic lists.
*/
