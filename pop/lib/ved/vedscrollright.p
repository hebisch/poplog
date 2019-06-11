/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/ved/vedscrollright.p
 > Purpose:         Scroll right a char
 > Author:          John Gibson, Oct 26 1991
 > Documentation:   REF *VEDPROCS
 */
compile_mode :pop11 +strict;

section;

define vedscrollright();
    vedscrollhorz(-1)
enddefine;

endsection;
