/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/frozval.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *PROCEDURE
 */

;;; ---------- ACCESSING/UPDATING CLOSURE FROZEN VALUES ------------------

#_INCLUDE 'declare.ph'

constant
        procedure Sys$-Check_closure
    ;

;;; ---------------------------------------------------------------------

section $-Sys => frozval;

define lconstant Frozval(n, clos, _updating);
    lvars clos, n, _updating;
    Check_integer(n, 1);
    Check_closure(clos);
    if _updating and clos!PD_FLAGS _bitst _:M_PD_CLOS_PROTECT
    and caller(2) >=@(w) _system_end then
        mishap(clos, 1, 'CAN\'T ALTER FROZVAL')
    endif;
    if _int(n) _gr clos!PD_CLOS_NFROZ then
        mishap(n, clos, 2, 'FROZVAL NUMBER OUT OF RANGE')
    endif;
    if _updating then
        () -> fast_frozval(n, clos)
    else
        fast_frozval(n, clos)
    endif
enddefine;

define frozval() with_nargs 2;
    Frozval(false)
enddefine;
;;;
define updaterof frozval() with_nargs 3;
    Frozval(true)
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  1 1988
        Moved out of procedure.p
 */
