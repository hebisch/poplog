/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_mc.p
 >  Purpose:        Move N characters to right, or left if N < 0 (default is 1)
 >  Author:         A.Sloman 1983 (see revisions)
 >  Documentation:  HELP *VEDCOMMS/ved_mc
 >  Related Files:  LIB * VED_DC  LIB * VED_ML
 */

section;

define global ved_mc();
    vedargint(vedargument) -> vedargument;
    sysrepeat(
        if vedargument > 0 then
            vedargument, vedcharright
        else
             negate(vedargument),vedcharleft
        endif)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Nov 11 1989
    Replaced vedargnum with vedargint
 */
