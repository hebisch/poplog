/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/exload_runtime_assign.p
 > Purpose:         Deferred accesses of exptrs and assignments to identifiers
 >                  etc, used by exload.
 > Author:          John Gibson, Jul 23 1992 (see revisions)
 */
compile_mode :pop11 +strict;

section;

    /* exptr_1, acc_p_1, id_1, ..., exptr_N, acc_p_N, id_N, N */
define global exload_runtime_assign(n);
    lvars n, id, acc_p;
    fast_repeat n times
        () -> id;
        unless id then
            ;;; Run the procedure on the exptr, no result
            fast_apply(/*exptr, acc_p*/)
        elseunless isident(id) then
            ;;; id is an extra arg for acc_p, which doesn't produce a result
            () -> acc_p;
            fast_apply(/*exptr,*/ id, acc_p)
        else
            ;;; Run the procedure on the exptr, and assign the result
            ;;; into the identifier
            fast_apply(/*exptr, acc_p*/) -> fast_idval(id)
        endunless
    endrepeat
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 11 1993
        Changed how it interprets the arguments
--- John Gibson, Sep 23 1992
        Now deals with multiple assignments
 */
