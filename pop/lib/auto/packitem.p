;;; --- Copyright University of Sussex 1986.  All rights reserved. ---------
;;; File:           $usepop/master/C.all/lib/auto/packitem.p
;;; Purpose:        Compress a list of items into a single word
;;; Author:         Ashamed (see revisions)
;;; Documentation:  HELP *PACKITEM
;;; Related Files:  LIB *UNPACKITEM

section;

define global procedure packitem(list) -> item;
    lvars item list;
    if isintegral(hd(list)) then
        0 -> item;
        until list == [] do
            item * 10 + (dest(list) -> list) -> item
        enduntil
    else
        cons_with consword {% applist(list, dest_characters) %} -> item
    endif
enddefine;

endsection;

;;; --- Revision History ---------------------------------------------------
;;; --- John Williams, Jan  7 1986  changed local vars to lvars,
;;;     uses DEST_CHARACTERS instead of ><
