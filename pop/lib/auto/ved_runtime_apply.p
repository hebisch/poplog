/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/ved_runtime_apply.p
 > Purpose:         List of "runtime actions" for Ved
 > Author:          John Williams, Jan 21 1993 (see revisions)
 > Documentation:   REF *ved_runtime_apply
 > Related Files:   C.all/lib/include/ved_declare.ph, C.all/ved/src/vdinitseq.p
 */
compile_mode :pop11 +strict;

section;

#_INCLUDE '$usepop/pop/lib/include/ved_declare.ph'

define ved_runtime_apply(procedure p);
    lvars rta;
    if testdef vedprocess and weakref[vedprocess] vedsetupdone then
        fast_apply(p)
    else
        sys_current_val("ident $-ved_runtime_actions") -> rta;
        if isinheap(rta) then
            rta nc_<> (p :: [])
        else
            ;;; copy anything already on it
            nonwriteable (rta <> (p :: []))
        endif -> sys_current_val("ident $-ved_runtime_actions")
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 29 1995
        Moved to lib/auto so it can be autoloaded at execute-level with Popc
--- John Gibson, Jun 16 1993
        Made to work with POPC at compile-time
 */
