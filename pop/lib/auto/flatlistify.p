/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 *  File:           C.all/lib/auto/flatlistify.p
 *  Purpose:        Given a nested structure returns a flat list with brackets
 *  Author:         Aaron Sloman, Sep 25 1985 (see revisions)
 *  Documentation:  HELP * FLATLISTIFY
 *  Related Files:
 */
compile_mode:pop11 +strict;

;;; A. Sloman Feb 1983
;;; given a list made of lists and vectors embedded arbitrarily
;;; return a list which contains all the words needed to create the list,
;;; if given to -popval-

section;

define global flatlistify(list);
    lvars list;

    define lconstant Flatlistify(x);
        lvars x;
        if islist(x) then
            "[", applist(x, Flatlistify), "]"
        elseif isvector(x) then
            "{", appdata(x, Flatlistify), "}"
        else
            x
        endif
    enddefine;

    maplist(list, Flatlistify)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul  3 1989
        Added +strict, tidied up
 */
