/*  --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:           C.all/lib/auto/expandlist.p
 > Purpose:        expand dynamic lists and make them static.
 > Author:         Mark Rubinstein & John Williams, Feb 11 1986 (see revisions)
 > Documentation:  HELP * EXPANDLIST
 > Related Files:  LIB * ISDYNAMIC
 */
compile_mode:pop11 +strict;

section;

define global expandlist(list) -> list;
    lvars l, b;
    if null(list) then
        [] -> list
    else
        list -> l;
        until null(fast_back(l) ->> b) do
            b -> l
        enduntil;
        if ispair(b) then
            [] -> fast_back(l)
        endif
    endif
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul  1 1989
        Added +strict
 */
