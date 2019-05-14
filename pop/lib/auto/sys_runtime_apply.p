/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/sys_runtime_apply.p
 > Purpose:         Apply or defer a runtime action procedure
 > Author:          John Gibson, May 15 1991 (see revisions)
 > Documentation:   REF *SYSTEM
 */
compile_mode :pop11 +strict;

#_INCLUDE '$usepop/pop/lib/include/vm_flags.ph'

section;

define global sys_runtime_apply(p);
    lvars procedure p, rta;
compile_mode :vm -prmprt;
    if pop_runtime then
        ;;; just apply it now
        fast_apply(p);
        if pop_runtime == "undef" then
            ;;; says that no actions have been run, so set it <true>
            ;;; (may also have the value "x" if X setup done)
            true -> pop_runtime
        endif
    else
        ;;; defer by adding it to the end of pop_runtime_actions
        ;;; (must access through sys_current_val for POPC)
        dlocal pop_vm_flags = pop_vm_flags fi_|| VM_NOPROT_PVARS;

        sys_current_val("ident $-pop_runtime_actions") -> rta;
        if isinheap(rta) then
            rta nc_<> (p :: [])
        else
            ;;; copy anything already on it
            nonwriteable (rta <> (p :: []))
        endif -> sys_current_val("ident $-pop_runtime_actions")
    endif
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 28 1992
        Uses sys_current_val to communicate with POPC correctly
--- John Gibson, Sep 30 1992
        Made it copy pop_runtime_actions if necessary
--- John Gibson, Jul  2 1992
        Made it assign true to pop_runtime only if currently "undef"
 */
