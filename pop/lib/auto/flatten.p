/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:           C.all/lib/auto/flatten.p
 > Purpose:        Flatten a tree into a list
 > Author:         Unknown, ??? (see revisions)
 > Documentation:
 > Related Files:  LIB * FAST_FLATTEN
 */
compile_mode:pop11 +strict;

section;

define flatten(item);
    lvars item;

    define lconstant Flatten(item);
        lvars item;
        if islist(item) then
            applist(item, Flatten)
        else
            item
        endif
    enddefine;

    [% Flatten(item) %]
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 15 1994
        Changed to test for islist and then use applist
--- John Gibson, Jul  3 1989
        Added +strict, tidied up
--- Mark Rubinstein, Feb 11 1986 - made to work on dynamic lists.
*/
