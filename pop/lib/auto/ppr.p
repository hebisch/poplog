/*  --- Copyright University of Sussex 1987.  All rights reserved. ---------
 >  File:           C.all/lib/auto/ppr.p
 >  Purpose:        Print list without brackets
 >  Author:         A.Sloman 1982 (see revisions)
 >  Documentation:  REF *PRINT, HELP * PPR
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define global ppr(item);
    lvars item;
    if islist(item) then
        applist(item, ppr)
    else
        spr(item)
    endif
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 27 1987
        Changed to use -islist- rather than -ispair-
 */
