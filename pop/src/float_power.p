/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/float_power.p
 > Purpose:
 > Author:          John Gibson 1988 (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;; ------------ ** FOR NON-INTEGRAL POWER OR FLOAT BASE ---------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'mathlib.ph'

constant
        procedure (Sys$-Cxf_**)
    ;

;;; -------------------------------------------------------------------------

section $-Sys;

    /*  ** for an non-integral power */
define **_frac_power(base, power);
    lvars base, power, _btype, _ptype, _base = _dfop1, _power = _dfresult;
    Dfloat(base, _base) -> _btype;
    Dfloat(power, _power) -> _ptype;
    if _neg(_btype) or _neg(_ptype) or _pfneg(_base) then
        ;;; either or both complex, or base negative -- result is complex
        if _neg(_ptype) then
            ;;; power is complex
            Dfloat(power!CX_REAL, _power) -> ;
            Dfloat(power!CX_IMAG, _dfradix) ->
        else
            _pfcopy(_dfradix, _0.0)
        endif;
        if _neg(_btype) then
            ;;; base is complex
            CXWEAK Cxf_op_cmplx(base, CXWEAK Cxf_**)
        elseunless _neg(_ptype) or testdef complex_key then
            mishap(base, 1, 'BASE >= 0 NEEDED IN ** (complex numbers not loaded)')
        else
            CXWEAK Cxf_op_real(_btype, CXWEAK Cxf_**)
        endif
    else
        ;;; both real and base >= 0
        if _pfzero(_base) then
            ;;; base = 0
            returnif(_pfzero(_power) or _pfneg(_power)) (false)
        else
            ;;; base /= 0
            _MATH1(_base, _f_LOG) -> ;
            returnunless(_pfmult(_base, _power) and _MATH1(_base, _f_EXP))
                                                                    (false)
        endif;
        Consdecimal(_btype, _base), true
    endif
enddefine;

define **_float_base(base, power, _was_+);
    lvars power, base, _res = _dfresult, _base = _dfop1, _btype, _was_+;
    if issimple(base) then
        _pf_dfloat_dec(base, _base), _:ARGTYPE_DECIMAL -> _btype
    else
        _pf_dfloat_ddec(base, _base), _:ARGTYPE_DDECIMAL -> _btype
    endif;
    _pfcopy(_res, _1.0);
    repeat
        if testbit(power, 0) then
            _pfmult(_res, _base);  IFNOT_ERR;
        endif;
        quitif((power >> 1 ->> power) == 0);
        _pfmult(_base, _base);  IFNOT_ERR;
    endrepeat;
    unless _was_+ then
        _pfcopy(_base, _res);
        _pfcopy(_res, _1.0);
        _pfdiv(_res, _base);    IFNOT_ERR;
    endunless;
    return(Consdecimal(_btype, _res), true);

    ERR: return(false)
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 25 1995
        Replaced _m*ath1 subroutine with _MATH1 macro
--- John Gibson, Feb  8 1989
        Fixed bug in **_frac_power where 0.0 base and power > 0 was
        causing library log function to be called (which printed an
        error message).
 */
