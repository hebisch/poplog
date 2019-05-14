/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/auto/cons.p
 >  Purpose:        provide non-syntax list constructer
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:  REF *LISTS
 */
compile_mode:pop11 +strict;

section;

define global cons = nonop ::(%%) enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 10 1992
        Made closure
 */
