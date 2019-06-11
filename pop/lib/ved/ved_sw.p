/*  --- Copyright University of Sussex 1994. All rights reserved. ----------
 >  File:           C.all/lib/ved/ved_sw.p
 >  Purpose:        Transpose 2 characters to left of cursor
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:
 */
compile_mode:pop11 +strict;

section;

define vars ved_sw();
    lvars   x, y;
    dlocal  vedstatic = true, vedcharinsert_attr = 0;
    if vedcolumn <= 2 then vederror('TOO CLOSE TO LEFT MARGIN') endif;
    vedcharleft();
    vedcurrentdchar() -> x;
    vedcharleft();
    vedcurrentdchar() -> y;
    vedcharinsert(x);
    vedcharinsert(y);
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jan 17 1994
        Fixed to preserve character attributes.
--- Mark Rubinstein, Nov 12 1985 - sectionised.
--- Jonathan Laventhol, April 1985 - modified to check cursor column first
 */
