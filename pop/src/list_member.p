/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/list_member.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *LISTS
 */

;;; --------------- TEST IF AN ITEM IS IN A LIST -------------------------

#_INCLUDE 'declare.ph'

;;; --------------------------------------------------------------------

define lmember(item, list);
    lvars list, item;
    until null(list) do
        returnif(fast_front(list) == item) (list);
        fast_back(list) -> list
    enduntil;
    false
enddefine;

define lmember_=(item, list);
    lvars list, item;
    until null(list) do
        returnif(fast_front(list) = item) (list);
        fast_back(list) -> list
    enduntil;
    false
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec  4 1995
        Replaced member with constant lmember_= returning list like lmember
        (member now autoloadable).
--- John Gibson, Apr  9 1988
        Moved out of old lists.p
 */
