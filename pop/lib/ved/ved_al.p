/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_al.p
 >  Purpose:        align each line of marked range with the left margin
 >  Author:         Chris Slymon (after D Roberts), June 1983
 >  Documentation:  HELP * FORMAT
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

/* aligns block of text with left margin */
define ved_al;
    lvars oldchanged=vedchanged;
    dlocal vedleftmargin, vedstatic, vedchanged, vedpositionstack;
    false ->> vedstatic -> vedchanged;
    vedpositionpush();
    vedmarkfind();
    if isprocedure( vedleftmargin) then
        vedleftmargin() -> vedleftmargin
    endif;
    until vedline fi_> vvedmarkhi do
        unless vvedlinesize = 0 then
            vedtextleft();
            if vedcolumn fi_<= vedleftmargin do
                until vedcolumn fi_> vedleftmargin do
                    vedcharinsert(` `)
                enduntil;
            elseif vedcolumn fi_> vedleftmargin fi_+ 1 then
                vedscreenleft();
                vedwordrightdelete()
            endif
        endunless;
        vednextline()
    enduntil;
    vedpositionpop();
    if oldchanged then oldchanged fi_+ 1 else 1 endif -> vedchanged;
enddefine;

endsection;
