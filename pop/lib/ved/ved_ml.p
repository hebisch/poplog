/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_ml.p
 >  Purpose:        move N lines. (defaults to 1)
 >  Author:         A.Sloman 1983 (see revisions)
 >  Documentation:  HELP * VEDCOMMS/ved_ml
 >  Related Files:
 */

;;; if N = 0 go to beginning of current line.
;;; If N < 0 go -N lines back
;;; else move N lines on

section;

define global ved_ml;
    vedargint(vedargument) -> vedargument;
    if vedargument == 0 then vedscreenleft()
    else
        sysrepeat(
            if vedargument < 0 then negate(vedargument), vedcharup
            else vedargument, vedchardown
            endif)
    endif
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Nov 11 1989
    Replaced vedargnum with vedargint

 */
