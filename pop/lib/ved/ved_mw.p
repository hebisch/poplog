/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_mw.p
 >  Purpose:        Move N words to right if N is positive, otherwise to left
 >  Author:         A.Sloman 1984 (see revisions)
 >  Documentation:  HELP * VEDCOMMS/ved_mw
 >  Related Files:
 */

section;

define global ved_mw();
    vedargint(vedargument) -> vedargument;
    sysrepeat(
        if vedargument >= 0 then vedargument, vedwordright
        else negate(vedargument), vedwordleft
        endif)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Nov 11 1989
    Replaced vedargnum with vedargint

 */
