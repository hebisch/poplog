/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 >  File:           C.all/lib/ved/vedconvertword.p
 >  Purpose:        Character conversion of specified number of words
 >  Author:         Aaron Sloman, May 1982 (see revisions)
 >  Documentation:  See comments
 >  Related Files:  LIB * VED_UCW, LIB * VED_LCW etc,
 */
compile_mode :pop11 +strict;

;;; For the given number of times do the following to the next word on
;;; right if flag is true, on left if flag is false
;;;     test each character in the word using test.
;;;     if it passes, then replace it with proc(char)

section;

define vedconvertword(test, proc, x, moveright);
    ;;; If moveright then go to right, otherwise to left, but reverse if
    ;;; x is negative.
    lvars
        char, oldchanged = vedchanged,
        procedure (test, proc),
        x, moveright, sline, scol, vcol, allspace;

    define lconstant isspace(char);
        lvars char = char fi_&& 16:FFFF;
        char == `\s` or char == `\t`
    enddefine;

    unless isinteger(x) or (strnumber(x) ->> x) then 1 -> x endunless;
    unless isinteger(x) then
        vederror('WORD COUNT: INTEGER NEEDED, not: ' sys_>< x)
    endunless;
    if x < 0 then
        negate(x) -> x;
        not(moveright) -> moveright
    endif;

    ;;; deal with starting off in the middle of a word
    unless vedatitemstart(vedcolumn, vedthisline(), vvedlinesize) then
        vedstartwordleft()
    endunless;

    ;;; change the case of x words, left or right
    until x == 0 do
        (vedcolumn, vedline) -> (scol, sline);
        if moveright then
            quitif(sline >= vvedbuffersize
                    and (sline /== vvedbuffersize or scol > vvedlinesize));
            vedendwordright();
            if vedline > sline then 1 -> scol endif;
            scol, vedcolumn -> (vedcolumn, scol)
        else
            vedstartwordleft();
            nextif(vedline < sline);
            vedcolumn -> vcol
        endif;

        true -> allspace;
        while vedcolumn < scol do
            vedcurrentdchar() -> char;
            unless isspace(char) then
                if test(char) then proc(char) -> vedcurrentdchar() endif;
                false -> allspace
            endunless;
            vedcolumn + 1 -> vedcolumn
        endwhile;
        unless moveright then vcol -> vedcolumn endunless;
        unless allspace then x - 1 -> x endunless
    enduntil;

    if oldchanged then oldchanged + 1 else 1 endif -> vedchanged
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec  1 1993
        Added test for end of file when moving right
--- John Gibson, Apr 28 1993
        Only adds 1 to vedchanged
--- John Gibson, Jan 19 1992
        Rewrote using vedendwordright/vedstartwordleft so that it works
        on WORDS, as it's supposed to. Also changed to work with dchars.
--- Aaron Sloman, Jan  2 1988
        Made much clearer (I hope).
        Fixed to work with negative x, so ved_ucw, and ved_lcw will work
        with negative arguments. Also fixed to treat tabs like spaces.
        Should probably also treat full-stops and other punctuation marks
        like spaces.
 */
