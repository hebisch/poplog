/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_gbl.p
 > Purpose:         Go to (move cursor) beginning of current list
 > Author:          John Williams, Oct 21 1988 (see revisions)
 > Documentation:
 > Related Files:   C.all/lib/ved/vedfindbracket.p
 */
compile_mode :pop11 +strict;

section;

define vars ved_gbl();
    if subsystem == "lisp" then
        vedfindbracket(`(`, `)`, vedatstart, vedcharleft)
    else
        vedfindbracket(`[`, `]`, vedatstart, vedcharleft)
    endif
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 13 1993
        popcom*piler -> subsystem
 */
