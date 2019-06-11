/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/ved/define_ved_runtime_action.p
 > Purpose:         "define form" for VED runtime actions
 > Author:          John Williams, Feb  5 1993 (see revisions)
 > Documentation:
 > Related Files:   LIB * DEFINE_RUNTIME_ACTION
 */
compile_mode :pop11 +strict;

section;

uses-by_name ved_runtime_apply;

define :define_form ved_runtime_action;
    lvars name;
    if (readitem() ->> name) == ";" then
        false -> name
    else
        name sys_>< nullstring -> name;
        pop11_need_nextitem(";") ->
    endif;
    sysPROCEDURE(name, 0);
    pop11_comp_stmnt_seq_to("enddefine") ->;
    sysENDPROCEDURE();
    sysPUSHQ();
    sysCALL("ved_runtime_apply")
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 29 1995
        Added uses-by_name
 */
