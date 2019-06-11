/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/ved/vedenderror.p
 >  Purpose:        give an EOF error
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vedenderror();
    vederror('\{b}end of file')
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Oct  4 1985 - sectionised and made a closure.
 */
