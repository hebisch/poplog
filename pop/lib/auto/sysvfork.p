/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.unix/lib/auto/sysvfork.p
 > Purpose:         Old version of sys_vfork
 > Author:          John Gibson, Apr 19 1994
 > Documentation:   REF * OBSOLETE
 */
compile_mode :pop11 +strict;

section;

define sysvfork = sys_vfork(%true%) enddefine;

endsection;
