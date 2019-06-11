/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_linemax.p
 >  Purpose:        for setting value of vedlinemax
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:
 */
compile_mode: pop11 +strict;

section;

define global vars ved_linemax();
    lvars new_linemax;
    dlocal vedleftmargin;

    if isprocedure(vedleftmargin) then
        vedleftmargin() -> vedleftmargin;
    endif;

    unless vedargument = nullstring do
        strnumber(vedargument) -> new_linemax;
        if new_linemax then
            if new_linemax > vedleftmargin then
                new_linemax-> vedlinemax;
            else
                vederror('LINEMAX MUST BE MORE THAN VEDLEFTMARGIN')
            endif
        else
            vederror('NUMBER NEEDED')
        endif;
    endunless;

    vedputmessage('VEDLINEMAX: ' sys_>< vedlinemax);

enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Sep  8 1992
        o Now compiles under +strict
        o Works properly when pop_pr_quotes is true
        o Tidied up
 */
