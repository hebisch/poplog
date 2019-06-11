/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_gel.p
 > Purpose:         Go to (move cursor) end of current list
 > Author:          John Williams, Oct 21 1988 (see revisions)
 > Documentation:
 > Related Files:   C.all/lib/ved/vedfindbracket.p
 */
compile_mode :pop11 +strict;

section;

define vars ved_gel();
    if subsystem == "lisp" then
        vedfindbracket(`)`, `(`, vedatend, vedcharnext)
    else
        vedfindbracket(`]`, `[`, vedatend, vedcharnext)
    endif
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 13 1993
        popcom*piler -> subsystem
 */
