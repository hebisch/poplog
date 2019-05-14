/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/auto/sys_raise_signal.p
 > Purpose:         Old name for sys_raise_ast
 > Author:          John Gibson, Apr 22 1994
 > Documentation:   REF * OBSOLETE
 */
compile_mode :pop11 +strict;

section;

define sys_raise_signal = sys_raise_ast(%%) enddefine;

endsection;
