/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_dl.p
 >  Purpose:        delete lines in ved, (specified number or default 1)
 >  Author:         A.Sloman 1984 (see revisions)
 >  Documentation:  HELP *VEDCOMMS/ved_dl
 >  Related Files:
 */

;;; ENTER DL - delete lines. Argument N defaults to 1
;;; If N = 0 do nothing
;;; If N < 0 and = -N then delete N lines up counting current line
;;; Else delete N lines down counting current line

section;

define global ved_dl();
    vedargint(vedargument) -> vedargument;
    returnif(vedargument == 0);
    vedlinedelete();
    if vedargument < 0 then
        repeat -vedargument-1 times
            vedcharup(), vedlinedelete()
        endrepeat
    else
        repeat vedargument-1 times
            vedlinedelete()
        endrepeat
    endif
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Nov 11 1989
    Replaced vedargnum with vedargint

--- John Gibson, Aug 13 1989
        Got rid of use of sysrepeat and procedure composition
 */
