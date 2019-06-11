/*  --- Copyright University of Sussex 1990.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_ljmr.p
 >  Purpose:        move all lines in marked range left by vedleftmargin places
 >  Author:         A.Sloman, about 100 years ago
 >  Documentation:  REF * VEDCOMMS/ved_ljmr  HELP * FORMAT
 >  Related Files:  LIB * VED_BL, LIB * VED_BR
 */

/*
Unlike ved_bl this will not object if some lines are already at the
left margin.

*/

section;

define global ved_ljmr();
    lvars cols, char, left = true, line, col;
    dlocal vedleftmargin, vedbreak = false;
    vedpositionpush();
    if strnumber(vedargument) ->> cols then
        if cols < 0 then
            false -> left;
            -cols -> cols
        else
            cols -> vedleftmargin
        endif
    else
        0 -> cols;
        if isprocedure(vedleftmargin) then
            vedleftmargin() -> vedleftmargin
        endif
    endif;

    if vedleftmargin == 0 then 9999 -> vedleftmargin endif;

    vedline -> line; vedcolumn -> col;
    vedmarkfind();

    until vedline > vvedmarkhi do
        1 -> vedcolumn;
        if left then
            ;;; Find where leading white space ends
            while vedcolumn <= min(vedleftmargin,vvedlinesize)
            and ((vedcurrentchar() ->> char) == `\s` or char == `\t`)
            do
                vedcharright()
            endwhile;
            if vedcolumn > 1 then vedclearhead() endif
        else
            repeat cols times vedcharinsert(`\s`) endrepeat
        endif;

        vedchardown();
    enduntil;
    vedjumpto(line,col);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Oct 28 1990
    Prevent localisation of vedline, vedcolumn. Can cause problems
    Generalised to allow negative argument for shifting text right.
    Fixed header
 */
