/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/subsystem_nest.p
 > Purpose:         Old name for subsystem_compile
 > Author:          John Gibson, Jan 13 1993
 > Documentation:   REF *OBSOLETE
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

define subsystem_nest = subsystem_compile(% false %) enddefine;

endsection;
