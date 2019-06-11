/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_pop.p
 > Purpose:         Exit VED to basewindow
 > Author:          John Gibson, Jan 12 1993 (see revisions)
 > Documentation:   REF *VEDCOMMS
 */
compile_mode :pop11 +strict;

include subsystem.ph;

section;

define vars ved_pop();
    lvars ssname, curr;
    if vedargument = nullstring then
        identfn
    elseunless (sys_compiler_subsystem(`T`) ->> curr) then
        vederror('NO TOP-LEVEL COMPILER RUNNING OUTSIDE VED')
    else
        consword(vedargument) -> ssname;
        subscr_subsystem(SS_COMPILER, ssname, vederror) -> ; ;;; check loaded
        procedure;
            ssname -> sys_compiler_subsystem(`t`)
        endprocedure
    endif;
    vedexit()
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jan  7 1994
        Fixed not to leave the subsystem name on the stack.
 */
