/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/ved/vedconvertline.p
 > Purpose:         Character conversion within line(s)
 > Author:          John Gibson, Feb 15 1992 (see revisions)
 > Documentation:   REF *VEDPROCS
 */
compile_mode :pop11 +strict;

section;

define global vedconvertline(test_p, convert_p, nlines);
    lvars c, oldchanged = vedchanged, nlines, procedure (test_p, convert_p);
    dlocal vedstatic = true, vedautowrite = false;
    unless isinteger(nlines) or (strnumber(nlines) ->> nlines) then
        1 -> nlines
    endunless;
    unless isinteger(nlines) then
        vederror('LINE COUNT: INTEGER NEEDED, NOT: ' sys_>< nlines)
    endunless;
    repeat nlines times
        vedscreenleft();
        while vedcolumn <= vvedlinesize do
            vedcurrentvedchar() -> c;
            if test_p(c) then
                vedcharinsert(convert_p(c))
            else
                vedcharright()
            endif
        endwhile;
        vednextline()
    endrepeat;
    if oldchanged then oldchanged + 1 else 1 endif -> vedchanged
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 30 1995
        Changed to use vedcurrentvedchar
 */
