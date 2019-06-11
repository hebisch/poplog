/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.unix/lib/ved/ved_qman.p
 > Purpose:         Quit current file and invoke MAN on another
 > Author:          John Williams, Mar 22 1989
 > Documentation:   HELP * MAN
 > Related Files:   LIB * VED_MAN
 */
compile_mode :pop11 +strict;

section;

define vars ved_qman();
    vedqget(ved_man)
enddefine;

endsection;
