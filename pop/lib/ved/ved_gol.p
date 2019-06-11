/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_gol.p
 > Purpose:         Go out of current list
 > Author:          John Williams, Oct 21 1988
 > Documentation:
 > Related Files:   C.all/lib/ved/vedfindbracket.p
 */
compile_mode :pop11 +strict;

section;

define vars ved_gol();
    ved_gbl();
    vedcharleft();
    ved_gbl();
enddefine;

endsection;
