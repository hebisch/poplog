/*  --- Copyright University of Sussex 1987.  All rights reserved. ---------
 > File:           C.all/lib/ved/vedfillblock.p
 > Purpose:        Insert a rectangular block of characters into VED buffer
 > Author:         Aaron Sloman, May 17 1987 (see revisions)
 > Documentation:  HELP * VEDFILLBLOCK
 > Related Files:  LIB * VEDCUTBLOCK, * VEDYANKBLOCK, * VEDREFRESHBLOCK
 */
compile_mode :pop11 +strict;

section;

define vedfillblock(line1, col1, line2, col2, char);
    ;;; Insert a rectangle filled with character char in the VED buffer.
    ;;; Move everything to the right if vedstatic is false
    lvars args, line1,col1,line2,col2,char,
        n = col2 - col1 + 1;    ;;; number of columns to be filled
    dlocal vedediting = false, vedbreak = false;

    vedpositionpush();
    vedjumpto(line1,col1);
    until vedline > line2 do
        col1 -> vedcolumn;
        repeat n times vedcharinsert(char) endrepeat;
        vedchardown();
    enduntil;
    vedpositionpop();
    ;;; chain out to get old value of editing
    chain(line1,col1,line2,col2, not(vedstatic),vedrefreshblock);
enddefine;


endsection;
