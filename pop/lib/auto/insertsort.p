/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 *  File:           $usepop/master/C.all/lib/auto/insertsort.p
 *  Purpose:        Insert-sort procedure
 *  Author:         Steven Hardy, August 1978 (see revisions)
 *  Documentation:
 *  Related Files:  LIB * SORT.P
 */

;;; Except on very short lists, this is slower than SORT.P

section;

define global insertsort(input, before) -> result;
lvars item tail list input procedure before result = [];
    until input == [] then
        dest(input) -> input -> item;
        if result == [] or before(item, front(result)) then
            conspair(item, result) -> result
        else
            result -> list;
            back(list) -> tail;
            until tail == [] or before(item, front(tail)) then
                tail -> list;
                fast_back(list) -> tail;
            enduntil;
            conspair(item, tail) -> back(list);
        endif;
    enduntil
enddefine;

endsection;
