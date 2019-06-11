/*  --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:           C.all/lib/ved/ved_dw.p
 > Purpose:        ENTER DW N  delete N words to right or -N words to left
 > Author:         A.Sloman, ??? (see revisions)
 > Documentation:
 > Related Files:
 */

section;

define global ved_dw();
    vedargint(vedargument) -> vedargument;
    sysrepeat(
        if vedargument > 0 then vedargument, vedwordrightdelete
        else negate(vedargument), vedwordleftdelete
        endif)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Nov 11 1989
    Replaced vedargnum with vedargint

--- Mark Rubinstein, Mar 14 1986 - Retrieved and sectionised.
*/
