/*  --- Copyright University of Sussex 1987. All rights reserved. ----------
 >  File:           C.all/lib/auto/sp.p
 >  Purpose:        print <arg> spaces
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:  REF *PRINT
 >  Related Files:  LIB * NL, *TABS.
 */
compile_mode:pop11 +strict;

section;

define global sp(n);
    lvars n;
    repeat n times cucharout(`\s`) endrepeat
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 27 1987
        Changed to do cucharout(`\s`) rather than pr(space)
 */
