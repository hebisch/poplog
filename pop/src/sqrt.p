/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/sqrt.p
 > Purpose:
 > Author:          John Gibson 1988 (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;; --------------------- SQUARE ROOT -------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'mathlib.ph'

;;; -----------------------------------------------------------------------

section $-Sys => sqrt;

define sqrt(x);
    lvars x, _argtype;
    if _neg(Dfloat(x, _dfop1) ->> _argtype) then
        ;;; arg is complex
        CXWEAK Cxf_op_cmplx(x, CXWEAK Cxf_sqrt) ->
    elseif _pfneg(_dfop1) then
        ;;; result is complex
        if testdef complex_key then
            CXWEAK Cxf_op_real(_argtype, CXWEAK Cxf_sqrt) ->
        else
            mishap(x, 1, 'NUMBER >= 0 NEEDED (complex numbers not loaded')
        endif
    else
        _MATH1(_dfop1, _f_SQRT) -> ;
        Consdecimal(_argtype, _dfop1)
    endif
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 25 1995
        Replaced _m*ath1 subroutine with _MATH1 macro
 */
