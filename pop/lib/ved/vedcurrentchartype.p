/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:          C.all/lib/ved/vedcurrentchartype.p
 >  Purpose:       Return type of character to left of cursor
 >  Author:        Chris Slymon, October 1983 (see revisions)
 >  Documentation:
 >  Related Files:
*/
compile_mode :pop11 +strict;

section;

define vedcurrentchartype;
    if vedcolumn - 1 > vvedlinesize then
        `\s`
    elseif vedcolumn == 1 then
        `\n`
    else
        vedchartype(fast_subscrs(vedcolumn - 1, fast_subscrv(vedline,vedbuffer)))
    endif
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Williams, Jan 22 1986
        changed to use 'vedchartype' for 'chartype'
*/
