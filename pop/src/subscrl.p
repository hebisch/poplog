/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/subscrl.p
 > Purpose:
 > Author:          John Williams (see revisions)
 > Documentation:   REF *LISTS
 */

;;; ------------------- SUBSCRIPTING LISTS ------------------------------

#_INCLUDE 'declare.ph'

constant
        procedure Sys$-Check_lsubscr
    ;

;;; --------------------------------------------------------------------

section $-Sys => subscrl fast_subscrl;

define subscrl() with_nargs 2;
    fast_front(Check_lsubscr());
enddefine;
;;;
define updaterof subscrl() with_nargs 3;
     -> fast_front(Check_lsubscr())
enddefine;

define fast_subscrl(n, list);
    lvars list, n;
    while n fi_> 1 do
        fast_back(list) -> list;
        n fi_- 1 -> n;
    endwhile;
    fast_front(list)
enddefine;
;;;
define updaterof fast_subscrl(item, n, list);
    lvars list, n, item;
    while n fi_> 1 do
        fast_back(list) -> list;
        n fi_- 1 -> n;
    endwhile;
    item -> fast_front(list)
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1988
        Moved out of lists.p
 */
