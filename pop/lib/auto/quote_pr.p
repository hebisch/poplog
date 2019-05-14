/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:           C.all/lib/auto/quote_pr.p
 > Purpose:        Print words or strings with appropriate quotes
 > Author:         Jon Cunningham (see revisions)
 > Documentation:  HELP *QUOTE_PR
 > Related Files:
*/
compile_mode :pop11 +strict;

section;

define quote_pr(item);
    lvars item;
    dlocal pop_pr_quotes = false;
    if isstring(item) then
        cucharout(`'`);
        appdata(item, cucharout);
        cucharout(`'`)
    elseif isword(item) then
        cucharout(`"`);
        appdata(item, cucharout);
        cucharout(`"`)
    else
        pr(item)
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 24 1995
        Tidied
 */
