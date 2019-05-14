/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/auto/rawoutflush.p
 >  Purpose:        flush the raw io channel
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:  HELP * RAWOUTFLUSH
 >  Related Files:
 */
compile_mode:pop11 +strict;

section;

define global vars rawoutflush();
    sysflush(poprawdevout)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct  8 1992
        Made vars for consistency with other raw- procedures
--- John Gibson, Nov 11 1987
        Replaced -popdevraw- with -poprawdevout-
 */
