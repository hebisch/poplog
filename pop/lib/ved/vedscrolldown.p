/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/ved/vedscrolldown.p
 > Purpose:         Scroll down a line
 > Author:          John Gibson, Oct 26 1991
 > Documentation:   REF *VEDPROCS
 */
compile_mode :pop11 +strict;

section;

define vedscrolldown();
    vedscrollvert(-1)
enddefine;

endsection;
