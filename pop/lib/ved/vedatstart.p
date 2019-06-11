/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/ved/vedatstart.p
 >  Purpose:        test if at beginning of file
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:  LIB * VEDATEND
 */
compile_mode :pop11 +strict;

section;

define vedatstart();
    vedline == 1 and vedcolumn == 1
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
