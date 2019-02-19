/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/list_assoc.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; ------------------ ASSOCIATION LIST ROUTINES -------------------------

#_INCLUDE 'declare.ph'

;;; ----------------------------------------------------------------------

section $-Sys;

define list_assoc(item, assoc_list);
    lvars item, assoc_list;
    until assoc_list == [] do
        if fast_front(assoc_list) == item then
            return(fast_back(assoc_list))
        else
            fast_back(fast_back(assoc_list)) -> assoc_list
        endif
    enduntil;
    false
enddefine;

define list_assoc_val(item, assoc_list);
    lvars item, assoc_list;
    until assoc_list == [] do
        if fast_front(assoc_list) == item then
            return(fast_front(fast_back(assoc_list)))
        else
            fast_back(fast_back(assoc_list)) -> assoc_list
        endif
    enduntil;
    false
enddefine;

define del_assoc_val(item, assoc_list);
    lvars item, assoc_list, _list = assoc_list, _last = false;
    repeat
        if _list == [] then return(false, assoc_list) endif;
        quitif(fast_front(_list) == item);
        fast_back(fast_back(_list) ->> _last) -> _list
    endrepeat;
    fast_back(_list) -> _list;
    fast_front(_list);      ;;; return value
    if _last then
        fast_back(_list) -> fast_back(_last);   ;;; remove entry
        assoc_list
    else
        fast_back(_list)
    endif
enddefine;

define cons_assoc() with_nargs 3;
    conspair(conspair())
enddefine;

define dest_assoc(assoc_list);
    lvars assoc_list;
    fast_front(assoc_list);
    fast_back(assoc_list) -> assoc_list;
    fast_front(assoc_list), fast_back(assoc_list)
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Williams, Jun 24 1988
        No longer exports -Lisp_list_assoc_val-
--- John Gibson, Mar 27 1988
        Moved out of lists.p
 */
