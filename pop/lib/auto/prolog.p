/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/prolog.p
 > Purpose:         Autoloads Prolog subsystem
 > Author:          Simon Nichols, Jul 17 1990 (see revisions)
 > Documentation:   HELP * PROLOG
 > Related Files:   C.all/prolog/src/prolog.p
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

uses subsystem;

unless is_subsystem_loaded("prolog") then
    subsystem_compile('$usepop/pop/plog/src/prolog.p', "pop11");
    sys_init_subsystem("prolog");
endunless;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 11 1992
        Added #_TERMIN_IF guard for POPC
 */
