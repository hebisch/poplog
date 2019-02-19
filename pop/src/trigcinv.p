/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/trigcinv.p
 > Purpose:
 > Author:          John Gibson 1988 (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;;----------------- INVERSE CIRCULAR TRIG FUNCTIONS -------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'mathlib.ph'

constant
        procedure (Sys$-Trig_inv_return, Sys$-Cxf_trigop_inv)
    ;

;;; ------------------------------------------------------------------------

section $-Sys => arcsin, arccos, arctan;

lconstant idstring = 'triginv-1arg';

define lconstant Trig_inv_1arg(x, _lib_routine, opsub);
    lvars x, opsub, _A = _dfop1, _argtype, _lib_routine;
    if _neg(Dfloat(x, _A) ->> _argtype) then
        ;;; arg is complex
        returnif(CXWEAK Cxf_op_cmplx(opsub, x, CXWEAK Cxf_trigop_inv))
    else
        ;;; for arcsin and arccos, result is complex if abs(x) > 1
        _pfcopy(_dfop2, _A);
        _pfabs(_dfop2);
        if _pfsgr(_dfop2, _1.0) then
            if testdef complex_key then
                returnif(CXWEAK Cxf_op_real(opsub, _argtype, CXWEAK Cxf_trigop_inv));
                goto ERR
            else
                mishap(x, 1, 'NUMBER ABS VALUE <= 1 NEEDED (complex numbers not loaded')
            endif
        endif;
        if _MATH1(_A, _lib_routine) then
            return(Trig_inv_return(_argtype, _A))
        endif
    endif;
ERR:
    Float_overflow(x, 1, false, idstring)
enddefine;

define arcsin() with_nargs 1;
    Trig_inv_1arg(_f_ASIN, TRIGOP_sin)
enddefine;

define arccos() with_nargs 1;
    Trig_inv_1arg(_f_ACOS, TRIGOP_cos)
enddefine;

define arctan(x);
    lvars x, _A = _dfop1, _argtype;
    if _neg(Dfloat(x, _A) ->> _argtype) then
        ;;; arg is complex
        returnif(CXWEAK Cxf_op_cmplx(TRIGOP_tan, x, CXWEAK Cxf_trigop_inv))
    else
        if _MATH1(_A, _f_ATAN) then
            return(Trig_inv_return(_argtype, _A))
        endif
    endif;
    Float_overflow(x, 1, false, idstring)
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 25 1995
        Replaced _m*ath1 subroutine with _MATH1 macro
 */
