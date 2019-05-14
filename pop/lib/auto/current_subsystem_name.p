/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/current_subsystem_name.p
 > Purpose:         Old subsystem procedure
 > Author:          John Gibson, Jan 14 1993
 > Documentation:   REF *OBSOLETE
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

define current_subsystem_name =
    sys_compiler_subsystem(% `c` %)
enddefine;

endsection;
