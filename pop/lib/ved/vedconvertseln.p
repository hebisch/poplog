/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/ved/vedconvertseln.p
 > Purpose:         Character conversion within Xved selection
 > Author:          John Williams, Oct 26 1994 (see revisions)
 > Documentation:   REF * VEDPROCS
 > Related Files:   LIB * VED_CHAT (the s option)
 */

compile_mode :pop11 +strict;

section;

global vars $-xved$-xvedselectionon;

global constant vedselectioncoords;

weak global constant procedure vedselection_adjust;


define vedconvertseln(test_p, convert_p);
    lvars procedure (test_p, convert_p), l1, c1, l2, c2, c;
    dlocal vedline, vedcolumn, vvedlinesize;

    unless wvedwindow
    and (wvedwindow == $-xved$-xvedselectionon) do
        vederror('No XVed selection in this file')
    endunless;
    explode(vedselectioncoords) -> (l1, c1, l2, c2);
    vedjumpto(l1, c1);

    until vedline fi_> l2 or vedatend() do
        repeat
            if vedline == l2 and vedcolumn fi_>= c2 then
                goto DONE
            endif;
            if vedcolumn fi_> vvedlinesize then
                vednextline();
                quitloop
            endif;
            if test_p(vedcurrentvedchar() ->> c) then
                convert_p(c) -> vedcurrentvedchar()
            endif;
            if vedcurrentchar() == `\t` then
                vedtabright()
            else
                vedcharright()
            endif
        endrepeat
    enduntil;
DONE:
    if testdef vedselection_adjust then
        weakref vedselection_adjust(false, false, false, false, true)
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 30 1995
        Changed to use vedcurrentvedchar
--- John Williams, Oct  2 1995
        Changed declaration for vedselectioncoords from vars to constant.
 */
