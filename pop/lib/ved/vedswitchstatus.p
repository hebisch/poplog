/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/vedswitchstatus.p
 > Purpose:         Old statusline toggle
 > Author:          John Gibson, Mar  3 1992
 > Documentation:   REF *OBSOLETE
 */
compile_mode :pop11 +strict;

section;

define global vedswitchstatus();
    not(ved_on_status) -> ved_on_status
enddefine;

endsection;
