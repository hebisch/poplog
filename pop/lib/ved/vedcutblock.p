/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:           C.all/lib/ved/vedcutblock.p
 > Purpose:        Cut or copy an arbitrary rectangular block of text in VED
 > Author:         Aaron Sloman, May 17 1987 (see revisions)
 > Documentation:  HELP * VEDCUTBLOCK
 > Related Files:  LIB * VEDREFRESHBLOCK, * VEDYANKBLOCK, * VEDFILLBLOCK
 */
compile_mode :pop11 +strict;

;;; Remove or copy a rectangular block of text from VED buffer
;;; and return it as a vector of strings

;;; N.B. vedediting is set false, so screen is not refreshed at all.
;;; see * VEDREFRESHBLOCK, if necessary.

section;

define lconstant extract_string(vedline,vedcolumn,col2,deleting) -> string;
    ;;; remove text between vedcolumn and col2 in current line, and
    ;;; return it in a string. Do not show anything on screen
    ;;; If deleting is false, merely copy, don't delete
    lvars col2, deleting, string, displayatend=false;
    dlocal vedline,vedcolumn,vvedlinesize,
        vedhardtabs = false, vedediting = false;

    vedsetlinesize();
    col2 + 1 - vedcolumn -> col2;   ;;; number of characters to be deleted

    ;;; can't use substring in case the range extends beyond the string
    consvedstring(#|
        until col2 == 0 do
            vedcurrentvedchar();
            if deleting then veddotdelete() else vedcharright() endif;
            col2 fi_- 1 -> col2;
        enduntil
    |#) -> string;
    vedtrimline();
enddefine;

define vedcutblock(line1,col1,line2,col2,deleting) ->vector;
    ;;; remove or copy characters in rectangle between line1,col1 and
    ;;; line2 col2 inclusive.
    ;;; Return a vector of strings removed or copied
    ;;; If deleting is false, then merely copy the characters, don't delete
    ;;; Doesn't show anything on the screen

    lvars line1, col1, line2, col2, vector, oldchanged=vedchanged, deleting;

    dlocal vedstatic=false, vedautowrite=false;

    ;;; make sure line1 < line2 and col1 < col2
    if line1 > line2 then line1,line2 -> line1->line2 endif;
    if col1 > col2 then col1,col2 -> col1->col2 endif;

    line2 fi_+ 1 -> line2;      ;;; for == limit check

    vedpositionpush();

    ;;; Remove stuff in the block and store in a vector of strings
    {%until line1 == line2 do
            extract_string(line1,col1,col2,deleting);
            line1 fi_+ 1 -> line1;
        enduntil%} -> vector;
    vedsetlinesize();
    vedpositionpop();
    if oldchanged then oldchanged fi_+ 1 else 1 endif -> vedchanged;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 10 1995
        Changed to use consvedstring/vedcurrentvedchar
--- Aaron Sloman, Jun 30 1991
        Made vedhardtabs false in extract_string, and updated documentation
 */
