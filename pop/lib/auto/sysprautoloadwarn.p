/*  --- Copyright University of Sussex 1991.  All rights reserved. ---------
 >  File:           C.all/lib/auto/sysprautoloadwarn.p
 >  Purpose:        Default auto-load 'warning' procedure
 >  Author:         Unknown (see revisions)
 >  Documentation:  HELP *PRAUTOLOADWARN
 */
compile_mode :pop11 +strict;

section;

define global sysprautoloadwarn() with_nargs 1;
    lconstant printfargs = writeable [0];
    -> fast_front(printfargs);
    printf(';;; AUTOLOADING %p\n', printfargs)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  3 1991
        Tidied up
 */
