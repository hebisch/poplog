/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/lib/ml.p
 > Purpose:         Load the Standard ML Subsystem
 > Author:          Robert John Duncan, May 16 1991 (see revisions)
 > Documentation:   HELP * PML
 > Related Files:   $usepop/pop/pml/src/ml.p
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

uses subsystem;

unless is_subsystem_loaded("ml") then
    subsystem_compile('$usepop/pop/pml/src/ml.p', "pop11");
    sys_init_subsystem("ml");
endunless;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 21 1992
        Added #_TERMIN_IF for popc
 */
