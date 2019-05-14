/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/auto/doprint.p
 >  Purpose:
 >  Author:         Unknown, ???
 >  Documentation:
 >  Related Files:
 */
compile_mode:pop11 +strict;

section;

define global vars macro doprint;
    pop11_compile(proglist);
    if stacklength() /== 0 then sysprarrow(true) endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 10 1992
        Cleaned up
 */
