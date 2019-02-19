/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/setfrontlist.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *LISTS
 */

;;; ---------------- BRING ITEM TO FRONT OF LIST -------------------------

#_INCLUDE 'declare.ph'

;;; -----------------------------------------------------------------------

    ;;; Bring item to front of list if its already in list, using same list
    ;;; links, otherwise join new element at front.
define setfrontlist(item, list);
    lvars list1, list2, item, list;
    unless list == [] then
        returnif(item == front(list)) (list);
        list -> list1;
        until (back(list1) ->> list2) == [] do
            if front(list2) == item then
                back(list2) -> back(list1);
                list -> back(list2);
                return(list2);
            else
                list2 -> list1
            endif
        enduntil
    endunless;
    conspair(item, list)
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 31 1988
        Moved out of util.p
 */
