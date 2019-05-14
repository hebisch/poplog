/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 >  File:           C.all/lib/auto/cis.p
 >  Purpose:        Produce cos(realnum) +: sin(realnum)
 >  Author:         John Gibson, Dec 19 1985 (see revisions)
 >  Documentation:  REF * NUMBERS
 */
compile_mode :pop11 +strict;

section;

define cis(realnum);
    lvars realnum;
    if isreal(realnum) then
        cos(realnum) +: sin(realnum)
    else
        mishap(realnum, 1, 'REAL NUMBER NEEDED')
    endif
enddefine;

sysprotect("cis");

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 19 1995
        Tidied
 */
