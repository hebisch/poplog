/* --- Copyright University of Sussex 1994.  All rights reserved. ---------
 > File:            C.all/lib/ved/vednextpara.p
 > Purpose:         Find the start and end of a paragraph in VED
 > Author:          Mark Rubinstein, May 7 1986 (see revisions)
 > Documentation:   REF * VEDPROCS
 > Related Files:   C.all/lib/ved/vedprevpara.p
                    C.all/lib/ved/vedprevparaend.p
                    C.all/lib/ved/vednextparaend.p
 */
compile_mode :pop11 +strict;

section;

define lconstant Is_para_start() -> yes;
    lvars yes = vedinparagraph();
    if yes == true and vedline > 1 then
        dlocal vedline, vedcolumn, vvedlinesize;
        vedcharup();
        not(vedinparagraph()) -> yes;
    endif;
enddefine;

define lconstant Is_para_end() -> yes;
    lvars yes = vedinparagraph();
    if yes then
        dlocal vedline, vedcolumn, vvedlinesize;
        vedchardown();
        vedinparagraph() /== true -> yes;
    endif;
enddefine;

define vednextpara();
    lvars (l, c) = (vedline, vedcolumn);
    repeat
        until Is_para_start() do
            if vedatend() then vedenderror() endif;
            vedchardown();
        enduntil;
        vedtextleft();
        quitif(vedline > l or vedcolumn > c);
        vedchardown();
    endrepeat;
enddefine;

define vednextparaend;
    lvars (l, c) = (vedline, vedcolumn);
    repeat
        until Is_para_end() do
            if vedatend() then vedenderror() endif;
            vedchardown();
        enduntil;
        vedtextright();
        quitif(vedline > l or vedcolumn > c);
        vedchardown();
    endrepeat;
enddefine;

define vedprevpara();
    lvars (l, c) = (vedline, vedcolumn);
    repeat
        until Is_para_start() do
            vedcharup();
        enduntil;
        vedtextleft();
        quitif(vedline < l or vedcolumn < c);
        vedcharup();
    endrepeat;
enddefine;

define vedprevparaend();
    lvars (l, c) = (vedline, vedcolumn);
    repeat
        until Is_para_end() do
            vedcharup();
        enduntil;
        vedtextright();
        quitif(vedline < l or vedcolumn < c);
        vedcharup();
    endrepeat;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jan 18 1994
        Modified to use vedinparagraph
--- Adrian Howard, Aug  1 1991
        - Tidied up
        - Fixed to take into account left margin
        - Fixed for cases cases when cursor on same line as para start
        - Added -vednextparaend- and -vedprevparaend-
--- Rob Duncan, May  8 1990
        Changed -vedlastpara- to -vedprevpara- (analogous to -vedprevline-).
--- Andreas Schoter, 6 Sept 1989
        Removed from LIB * VEDEMACS and made autoloadable.
 */
