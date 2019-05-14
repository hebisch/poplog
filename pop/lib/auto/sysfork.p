/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.unix/lib/auto/sysfork.p
 > Purpose:         Old version of sys_fork
 > Author:          John Gibson, Apr 19 1994
 > Documentation:   REF * OBSOLETE
 */
compile_mode :pop11 +strict;

section;

define sysfork = sys_fork(%true%) enddefine;

endsection;
