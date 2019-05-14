/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 >  File:           C.all/lib/auto/ncmaplist.p
 >  Purpose:        destructively apply a procedure to each item in a list
 >  Author:         John Williams, Jul 1985 (see revisions)
 >  Documentation:  HELP * NCMAPLIST
 >  Related Files:  LIB * NCMAPDATA
 */
compile_mode :pop11 +strict;

section;

define ncmaplist(list, procedure pdr) -> result;
    list -> result;
    until null(list) do
        pdr(fast_front(list)) -> fast_front(list);
        fast_back(list) -> list
    enduntil
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, May 26 1995
        Tidied up
 */
