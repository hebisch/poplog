/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 >  File:           C.all/lib/auto/nl.p
 >  Purpose:        print <arg> newlines
 >  Author:         A.Sloman 1982 (see revisions)
 >  Documentation:  HELP * NL
 >  Related Files:  LIB *SP and *TABS
 */

compile_mode :pop11 +strict;

section;

define global nl(n);
    lvars n;
    fast_repeat fi_check(n, false, false) times
        cucharout(`\n`)
    endfast_repeat
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, May 30 1995
        Now checks properly that the argument is an integer (previous
        version did "n - 1 ->;" which is ugly and *wrong*).
--- John Gibson, Nov  3 1991
        Tidied up
 */
