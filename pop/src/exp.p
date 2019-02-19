/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/exp.p
 > Purpose:
 > Author:          John Gibson 1988 (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;; --------------------- EXPONENT & LOG -------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'mathlib.ph'

;;; ------------------------------------------------------------------------

section $-Sys => log, exp;

define log(x);
    lvars x, _argtype;
    if _neg(Dfloat(x, _dfop1) ->> _argtype) then
        ;;; arg is complex -- result on stack if OK
        returnif(CXWEAK Cxf_op_cmplx(x, CXWEAK Cxf_log)) ()
    elseif _pfneg(_dfop1) then
        ;;; result is complex
        if testdef complex_key then
            return(CXWEAK Cxf_op_real(_argtype, CXWEAK Cxf_log) ->)
        else
            mishap(x, 1, 'NUMBER > 0 NEEDED (complex numbers not loaded')
        endif
    elseif _MATH1(_dfop1, _f_LOG) then
        return(Consdecimal(_argtype, _dfop1))
    endif;
    mishap(x, 1, 'NUMBER /= 0 NEEDED')
enddefine;

define exp(x);
    lvars x, _argtype;
    if _neg(Dfloat(x, _dfop1) ->> _argtype) then
        ;;; arg is complex -- result on stack if OK
        returnif(CXWEAK Cxf_op_cmplx(x, CXWEAK Cxf_exp)) ()
    elseif _MATH1(_dfop1, _f_EXP) then
        return(Consdecimal(_argtype, _dfop1))
    elseif _pfneg(_dfop1) then
        ;;; return 0 for any negative arg
        return(Consdecimal(_argtype, _0.0d0))
    endif;
    Float_overflow(x, 1, false, 'arith-exp')
enddefine;


endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 25 1995
        Replaced _m*ath1 subroutine with _MATH1 macro
--- John Gibson, Jul 31 1991
        Added test to -exp- for negative arg after error, returning 0
 */
