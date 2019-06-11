/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/ved/vedscrollleft.p
 > Purpose:         Scroll left a char
 > Author:          John Gibson, Oct 26 1991
 > Documentation:   REF *VEDPROCS
 */
compile_mode :pop11 +strict;

section;

define vedscrollleft();
    vedscrollhorz(1)
enddefine;

endsection;
