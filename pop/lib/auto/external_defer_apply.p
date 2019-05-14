/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/auto/external_defer_apply.p
 > Purpose:         Defer an action from inside external callback
 > Author:          John Gibson, Apr 13 1994 (see revisions)
 > Documentation:   REF * EXTERNAL
 */
compile_mode :pop11 +strict;

section;

include ast.ph;

define external_defer_apply(p);
    lvars p;
    lconstant
        flags   = ASTP_ERROR_DELETE || ASTP_BLOCK_RECURSIVE
                    || ASTP_BLOCK_IN_EXTERNAL || ASTP_TEMP_PAIR,
        cflags  = flags || ASTP_TEMP_CLOSURE;

    if isinteger(p) then
        ;;; arguments are as for consclosure
        consclosure(p), cflags
    else
        p, flags
    endif;
    sys_raise_ast(conspair())
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 20 1996
        Replaced pair cache with use of new ASTP_TEMP_PAIR flag (which
        causes the pair to be garbaged after use). Also allowed for
        automatic closure creation (which closure is garbaged by virtue of
        new ASTP_TEMP_CLOSURE flag).
 */
