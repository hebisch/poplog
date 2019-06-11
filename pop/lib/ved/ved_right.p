/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_right.p
 > Purpose:         Puts the string at right of current line as defined by vedlinemax
 > Author:          A.Sloman, 1983?? (see revisions)
 > Documentation:   HELP * VEDCOMMS/ved_right
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define vars ved_right();
    lvars start;
    dlocal vedargument;
    if vedargument == nullstring then
        vedtrimline();
        copy(vedthisline()) -> vedargument;
        vedscreenleft(); vedcleartail();
    endif;
    vedlinemax - length(vedargument) + 1 -> start;
    if start <= vvedlinesize then
        vederror('STRING TOO LONG FOR RIGHT')
    else
        vedtextright();
        until vedcolumn = start do vedcharright() enduntil;
        vedinsertstring(vedargument)
    endif;
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- James Goodlet, Feb 28 1989 - dlocalised vedargument.
--- Richard Bignell, Aug 19 1986 - made ved_right global.
--- Mark Rubinstein, Oct  4 1985 - sectionised and lvarsed.
 */
