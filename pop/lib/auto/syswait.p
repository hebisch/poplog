/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/auto/syswait.p
 > Purpose:         Old version of sys_wait
 > Author:          John Gibson, Apr 19 1994
 > Documentation:   REF * OBSOLETE
 */
compile_mode :pop11 +strict;

section;

define syswait();
    sys_wait(false) -> pop_status
enddefine;

endsection;
