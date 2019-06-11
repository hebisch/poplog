/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/ved/vededitor.p
 > Purpose:         Old procedure replaced by vededit
 > Author:          John Gibson, Mar  7 1994
 > Documentation:   REF * OBSOLETE
 */
compile_mode :pop11 +strict;

section;

define vars vededitor(defaults_p, file);
    lvars defaults_p, file;
    vededit(file, defaults_p)
enddefine;

endsection;
