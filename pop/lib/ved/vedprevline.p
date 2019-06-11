/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/vedprevline.p
 > Purpose:         Move cursor to start of previous line
 > Author:          John Williams, Oct  6 1989
 > Documentation:   REF * VEDPROCS
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define vedprevline();
    vedscreenleft();
    vedcharup()
enddefine;

endsection;
