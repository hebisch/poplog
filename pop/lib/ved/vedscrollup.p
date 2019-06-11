/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/ved/vedscrollup.p
 > Purpose:         Scroll up a line
 > Author:          John Gibson, Oct 26 1991
 > Documentation:   REF *VEDPROCS
 */
compile_mode :pop11 +strict;

section;

define vedscrollup();
    vedscrollvert(1)
enddefine;

endsection;
