/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/ved/vedgetfile.p
 > Purpose:         Old Ved procedure for getting a file
 > Author:          John Gibson, Mar  4 1994
 > Documentation:   REF * OBSOLETE
 */
compile_mode :pop11 +strict;

section;

define vedgetfile(defaults_p, file);
    lvars defaults_p, file;
    vededit(file, defaults_p)
enddefine;

endsection;
