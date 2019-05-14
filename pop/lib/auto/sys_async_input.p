/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/auto/sys_async_input.p
 > Purpose:         Closure of sys_async_io for input
 > Author:          John Gibson, Jul  8 1994
 > Documentation:   REF * SYSIO
 */
compile_mode :pop11 +strict;

section;

define sys_async_input = sys_async_io(% 0 %) enddefine;

endsection;
