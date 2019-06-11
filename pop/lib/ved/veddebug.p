/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/ved/veddebug.p
 >  Purpose:        Used for debugging and testing editor system procedures.
 >  Author:         A.Sloman, 44 BC? (see revisions)
 >  Documentation:  HELP * VEDPROCS
 >  Related Files:
 */

;;; The input should be a string. It is printed out on the status line
;;; The system pauses until a character is typed. If it is RETURN, then
;;; programs continue running, otherwise popready is called.
section;

define global veddebug(x);
    lvars x, file;
    dlocal vedediting=true;
    vedscreenbell();
    vedputmessage(x);
    vedwiggle(vedline,vedcolumn);
    vedinascii() -> x;
    unless x == `\r` then
        pr('VEDDEBUG:\n');
        valof("popready")();
        false -> vedprintingdone;
        vedscreenraw();
        1000 ->> vedscreenline -> vedscreencolumn;
        vedrestorewindows();
    endunless;
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Aaron Sloman, Aug 28 1988
    Altered to make it restore the screen properly after calling
    popready.
    Altered to make cursor wiggle at current line and current column
    Postpones compilation of popready until needed.
--- Aaron Sloman, Apr 27 1986
    Altered to make vedediting true locally - otherwise doesn't work.
 */
