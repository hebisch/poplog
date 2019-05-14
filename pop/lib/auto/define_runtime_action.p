/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/define_runtime_action.p
 > Purpose:         "define form" for runtime actions
 > Author:          John Williams, Feb  5 1993
 > Documentation:
 > Related Files:   LIB * DEFINE_VED_RUNTIME_ACTION
 */

section;

define :define_form global runtime_action;
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
    sysCALL("sys_runtime_apply")
enddefine;


endsection;
