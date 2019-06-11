/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_lcol.p
 >  Purpose:        Change the value of the leftmost column (margin)
 >  Author:         David Roberts, 1983 (see revisions)
 >  Documentation:  REF *ved_lcol
 >  Related Files:  LIB * VED_RCOL
*/
compile_mode :pop11 +strict;

section;

define vars ved_lcol;
    lvars col;
    unless vedargument = '?' do
        if (strnumber(vedargument) ->> col) then
            if col > 0 then
                col - 1 -> vedleftmargin
            else
                vederror('\{b}left margin width must be >= 0')
            endif
        else
            vedcolumn - 1 -> vedleftmargin
        endif
    endunless;
    ;;; Now report new left-most column
    (if isprocedure(vedleftmargin) then
        vedleftmargin()
    else
        vedleftmargin
    endif) + 1 -> col;
    vedputmessage('\{b}leftmost column is ' <> (col sys_>< nullstring))
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, May  1 1995
        Now copes with vedleftmargin being a procedure (cf. BR davidy.93).
--- John Williams, Sep 18 1992
        Changed >< to sys_><
 */
