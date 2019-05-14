/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/auto/readtill.p
 >  Purpose:        read in items until specified terminator is read
 >  Author:         Aaron Sloman, Mar 1982 (see revisions)
 >  Documentation:  HELP * READTILL
 >  Related Files:
 */
compile_mode :pop11 +strict;

;;; read in text items using readitem, until a specified terminator or member
;;; of a list of specified terminators is reached. Return the terminator
;;; found. All items are left on the stack. Macros are not expanded.

section;

define global readtill(x) -> y;
    lvars x y;
    until (readitem() ->> y) == x or ispair(x) and lmember(y,x) do
        y
    enduntil;
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Oct 17 1992
        Tidied up
--- Mark Rubinstein, Sep 26 1985 - lvarsed and sectioned.
 */
