/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/switch_subsystem_to.p
 > Purpose:         Old procedure to change current compiler subsystem
 > Author:          John Gibson, Jan 12 1993
 > Documentation:   REF *OBSOLETE
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

define switch_subsystem_to() with_nargs 1;
    () -> sys_compiler_subsystem(`c`)
enddefine;

endsection;
