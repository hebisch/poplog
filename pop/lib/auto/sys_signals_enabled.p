/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/auto/sys_signals_enabled.p
 > Purpose:         Old name for pop_asts_enabled
 > Author:          John Gibson, Apr 22 1994
 > Documentation:   REF * OBSOLETE
 */
compile_mode :pop11 +strict;

section;

define active sys_signals_enabled =
    nonactive pop_asts_enabled(%%)
enddefine;

endsection;
