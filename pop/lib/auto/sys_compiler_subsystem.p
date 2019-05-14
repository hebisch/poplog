/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/sys_compiler_subsystem.p
 > Purpose:         Get/change current or toplevel compiler subsystem
 > Author:          John Gibson, Jan 12 1993 (see revisions)
 > Documentation:   REF *SUBSYSTEM
 */
compile_mode :pop11 +strict;

include ved_declare.ph;
include subsystem.ph;

section;

lconstant
    pcs_id = ident pop_compiler_subsystem,
    macro IN_VEDPROCESS =
        [(testdef vedprocess and weakref[vedprocess] vedinvedprocess)],
;

define lconstant Compiler_subsystem(which);
    lvars which, psc, in_vedsetpop, start_frame;
    if which == `T` or which == `t` then
        ;;; Top outside VED, or top inside/outside VED
        pop_setpop_compiler -> psc;
        if IN_VEDPROCESS then
            if iscaller(weakref vedsetpop) ->> in_vedsetpop then
                if which == `T` then
                    caller_valof(ident pop_setpop_compiler,
                                            in_vedsetpop fi_+ 1) -> psc
                endif
            else
                returnunless(which == `T`) (false, false)
            endif
        endif;
        iscaller(psc) -> start_frame;
        returnunless(start_frame and isdlocal(pcs_id, psc)) (false, false)
    else
        ;;; Current outside VED, or current inside/outside VED
        if which == `C` and IN_VEDPROCESS then
            iscaller(weakref vedprocess) fi_+ 1
        else
            ;;; `c` (or anything else)
            1
        endif -> start_frame
    endif;
    caller_valof(pcs_id, start_frame), start_frame
enddefine;

define sys_compiler_subsystem(/*which*/) with_nargs 1;
    Compiler_subsystem() ->
enddefine;
;;;
define updaterof sys_compiler_subsystem(ssname, which);
    lvars   ssname, which, frame, p, curr, newcomp;

    if which == `T` or which == `C` then
        ;;; Code below will work for these, but it doesn't make a lot of
        ;;; sense to switch the outer subsystem from within VED (except
        ;;; with ved_pop).
        mishap(ssname, 1, 'sys_compiler_subsystem: `T` OR `C` NOT ALLOWED FOR UPDATER')
    elseunless (Compiler_subsystem(which) -> frame) ->> curr then
        mishap(ssname, 1, 'sys_compiler_subsystem: NO CORRESPONDING COMPILER SUBSYSTEM TO REPLACE')
    else
        if which == `t` and ssname == "prolog" then "top" -> ssname endif;
        returnif(curr == ssname)
    endif;
    subscr_subsystem(SS_COMPILER, ssname) -> newcomp;

    frame fi_- 1 -> frame;      ;;; adjust for exit from Compiler_subsystem
    while (caller(frame) ->> p) and not(isdlocal(pcs_id, p)) do
        frame fi_+ 1 -> frame
    endwhile;

    unless p then
        mishap(0, 'SYSTEM ERROR IN sys_compiler_subsystem')
    endunless;

    ;;; first compiler procedure must be caller(frame-1), and the environment
    ;;; of this procedure must contain the appropriate cucharin

    define lconstant run_new(ssname, newcomp);
        lvars ssname, newcomp;
compile_mode :vm -prmprt;
        ssname ->> subsystem -> pop_compiler_subsystem;
        ;;; caller(1) is the old compiler, caller(2) the procedure
        ;;; with pop_compiler_subsystem dlocal
        if caller(2) == pop_setpop_compiler and cucharin == charin
        and not(IN_VEDPROCESS) then
            ;;; call this only for the topmost compiler
            subsystem_changed_warn(subsystem)
        endif;
        chain(cucharin, newcomp, chain)
    enddefine;

    chainto(ssname, newcomp, callstacklength(frame fi_- 1), run_new)
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, May  2 1993
        Fixed missing lconstant on Compiler_subsystem
 */
