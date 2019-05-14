/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/auto/sys_signal_queue.p
 > Purpose:         Old name for pop_ast_queue
 > Author:          John Gibson, Apr 22 1994
 > Documentation:   REF * OBSOLETE
 */
compile_mode :pop11 +strict;

section;

define active sys_signal_queue =
    nonactive pop_ast_queue(%%)
enddefine;

endsection;
