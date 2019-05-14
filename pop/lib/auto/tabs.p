/*  --- Copyright University of Sussex 1987. All rights reserved. ----------
 >  File:           C.all/lib/auto/tabs.p
 >  Purpose:        print <arg> tabs.
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:  REF *PRINT
 >  Related Files:  LIB * NL, *SP
 */
compile_mode:pop11 +strict;

section;

define global tabs(n);
    lvars n;
    repeat n times cucharout(`\t`) endrepeat
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- John Gibson, Jul 27 1987
        Changed to use cucharout(`\t`) rather than pr(tab)
--- Mark Rubinstein, Sep 27 1985
        lvarsed and sectioned.
 */
