/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_dc.p
 >  Purpose:        delete <arg> characters (to left if negative)
 >  Author:         A.Sloman 1983? (see revisions)
 >  Documentation:
 >  Related Files:
 */

section;

define global ved_dc();
    vedargint(vedargument) -> vedargument;
    sysrepeat(
        if vedargument >= 0 then vedargument, veddotdelete
        else negate(vedargument), vedchardelete
        endif)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Nov 11 1989
    Replaced vedargnum with vedargint
 */
