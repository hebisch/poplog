/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/is_subsystem_loaded.p
 > Purpose:         Test whether a subsystem is loaded
 > Author:          John Williams, Apr 30 1990 (see revisions)
 > Documentation:   REF *SUBSYSTEM
 */
compile_mode :pop11 +strict;

include subsystem.ph;

section;

define is_subsystem_loaded(ssname);
    lvars field = SS_COMPILER;
    if isboolean(ssname) then
        ;;; true means include those with compiler not loaded
        if ssname then SS_NAME -> field endif;
        () -> ssname
    endif;
    subscr_subsystem(field, ssname,
                            procedure(errms);
                                lvars errms;
                                exitfrom(false, is_subsystem_loaded)
                            endprocedure) -> ;
    true
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 11 1993
        Changes for subsystem overhaul.
--- John Gibson, Dec  2 1992
        Made sys_subsystem_table separately autoloadable
--- John Williams, Nov 14 1991
        No longer uses the matcher (at Aaron's request)
 */
