/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/trighinv.p
 > Purpose:
 > Author:          John Gibson 1988 (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;;-------------- INVERSE HYPERBOLIC TRIG FUNCTIONS ---------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'mathlib.ph'

section $-Sys;

constant
        procedure (Trig_inv_return, Cxf_trigop_inv),
        _log_2
    ;

endsection;

;;; -----------------------------------------------------------------------

section $-Sys => arcsinh arccosh arctanh;

lconstant idstring = 'triginv-1arg';

define arcsinh(x);
    lvars x, neg, _A = _dfop1, _X = _dfop2, _argtype;
    if _neg(Dfloat(x, _A) ->> _argtype) then
        ;;; complex arg
        returnif(CXWEAK Cxf_op_cmplx(TRIGOP_sinh, x, CXWEAK Cxf_trigop_inv))
    else
        ;;; compute real arcsinh = log(x + sqrt(x*x + 1))
        if _pfneg(_A) ->> neg then _pfnegate(_A) endif;
        if _pfsgr(_A, _1.0) then
            _pfcopy(_X, _A);
            if _pfmult(_X, _X) and _pfadd(_X, _1.0) then
                _MATH1(_X, _f_SQRT) -> ;
                _pfadd(_A, _X) -> ;
                _MATH1(_A, _f_LOG) -> ;
            else
                _MATH1(_A, _f_LOG) -> ;
                _pfadd(_A, _log_2) -> ;
            endif
        else
            _pfcopy(_X, _1.0), _pfdiv(_X, _A) -> ;
            if _pfmult(_X, _X) and _pfadd(_X, _1.0) then
                _MATH1(_X, _f_SQRT) -> ;
                _pfadd(_X, _1.0) -> ;
                _pfmult(_A, _X) -> ;
                _MATH1(_A, _f_LOG) -> ;
            else
                _pfcopy(_A, _0.0)
            endif
        endif;
        if neg then _pfnegate(_A) endif;
        return(Trig_inv_return(_argtype, _A))
    endif;
    Float_overflow(x, 1, false, idstring)
enddefine;

define arccosh(x);
    lvars x, _A = _dfop1, _X = _dfop2, _argtype;
    if _neg(Dfloat(x, _A) ->> _argtype) then
        ;;; complex arg
        returnif(CXWEAK Cxf_op_cmplx(TRIGOP_cosh, x, CXWEAK Cxf_trigop_inv))
    elseunless _pfsgreq(_A, _1.0) then
        if testdef complex_key then
            returnif(CXWEAK Cxf_op_real(TRIGOP_cosh, _argtype, CXWEAK Cxf_trigop_inv))
        else
            mishap(x, 1, 'NUMBER >= 1 NEEDED (complex numbers not loaded')
        endif
    else
        ;;; compute real arccosh = log(x + sqrt(x*x - 1))
        _pfcopy(_X, _A);
        if _pfmult(_X, _X) then
            _pfsub(_X, _1.0) -> ;
            _MATH1(_X, _f_SQRT) -> ;
            _pfadd(_A, _X) -> ;
            _MATH1(_A, _f_LOG) -> ;
        else
            _MATH1(_A, _f_LOG) -> ;
            _pfadd(_A, _log_2) -> ;
        endif;
        return(Trig_inv_return(_argtype, _A))
    endif;
    Float_overflow(x, 1, false, idstring)
enddefine;

define arctanh(x);
    lvars x, _A = _dfop1, _X = _dfop2, _argtype;
    if _neg(Dfloat(x, _A) ->> _argtype) then
        ;;; complex arg
        returnif(CXWEAK Cxf_op_cmplx(TRIGOP_tanh, x, CXWEAK Cxf_trigop_inv))
    elseif _pfsgr(_A, _1.0) or _pfsgr(_-1.0, _A) then
        if testdef complex_key then
            returnif(CXWEAK Cxf_op_real(TRIGOP_tanh, _argtype, CXWEAK Cxf_trigop_inv))
        else
            mishap(x, 1, 'NUMBER ABS VALUE < 1 NEEDED (complex numbers not loaded')
        endif
    else
        ;;; compute real arctanh = log((1+x)/(1-x)) / 2
        _pfcopy(_X, _1.0);
        _pfsub(_X, _A) -> ;
        _pfadd(_A, _1.0) -> ;
        if _pfdiv(_A, _X) and _MATH1(_A, _f_LOG) then
            _pfmult(_A, _0.5) -> ;
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
