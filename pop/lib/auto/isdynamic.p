/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/auto/isdynamic.p
 >  Purpose:        for recognising dynamic lists
 >  Author:         Aaron Sloman, Dec 1983 (see revisions)
 >  Documentation:  HELP * ISDYNAMIC
 >  Related Files:
 */
compile_mode:pop11 +strict;

;;; - Return false or generating procedure

section;

define global isdynamic(x);
lvars x y;
    unless ispair(x) do
        false
    else
        while ispair(fast_back(x)->> x) do endwhile;
        if isprocedure(x) then
            x
        else false
        endif
    endunless
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- John Gibson, Oct 10 1992
        Cleaned up
--- Mark Rubinstein, Feb 11 1986 - made global
--- Mark Rubinstein, Oct 29 1985 - sectionised and lvarsed.
 */
