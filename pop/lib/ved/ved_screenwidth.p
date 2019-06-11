/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/ved/ved_screenwidth.p
 >  Purpose:        for setting value of vedscreenwidth
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:
 */
compile_mode: pop11 +strict;

section;

define global vars ved_screenwidth();
    if vedargument == nullstring then
    elseif strnumber(vedargument)  then
        strnumber(vedargument)  -> vedscreenwidth;
    else vederror('NUMBER NEEDED')
    endif;
    vedputmessage('VEDSCREENWIDTH : ' sys_>< vedscreenwidth)
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Adrian Howard, Sep  8 1992
        Changed to use +strict and sys_><
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
