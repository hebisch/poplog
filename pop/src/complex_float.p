/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/complex_float.p
 > Purpose:
 > Author:          John Gibson, Jan 24 1988 (see revisions)
 */

;;; ---------------- FLOATING-POINT COMPLEX NUMBERS --------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'mathlib.ph'


section $-Sys;

define Cxf_dec_->_ddec() with_nargs 1;
    dlocal popdprecision = true;
    _pf_dfloat_dec((), _dfop1);
    Consdecimal(_:ARGTYPE_DDECIMAL, _dfop1)
enddefine;

define Get_df_complex(_argtype, _real, _imag);
    lvars _argtype, _real, _imag;
    Get_complex(Consdecimal(_argtype, _real), Consdecimal(_argtype, _imag))
enddefine;


;;; --- COMPLEX FLOATING ARITHMETIC -------------------------------------------

define Cxf_copy(_A, _B, _C, _D);
    lvars _A, _B, _C, _D;
    _pfcopy(_A, _C), _pfcopy(_B, _D)
enddefine;

    /*  _A + _C -> _A,  _B + _D -> _B
    */
define Cxf_add(_A, _B, _C, _D);
    lvars _A, _B, _C, _D;
    _pfadd(_A, _C) and _pfadd(_B, _D)
enddefine;

    /*  _A - _C -> _A,  _B - _D -> _B
    */
define Cxf_sub(_A, _B, _C, _D);
    lvars _A, _B, _C, _D;
    _pfsub(_A, _C) and _pfsub(_B, _D)
enddefine;

    /*  _A * _C - _B * _D -> _A
        _B * _C + _A * _D -> _B
    */
define Cxf_mult(_A, _B, _C, _D);
    lvars _A, _B, _C, _D;
    lstackmem dfloat _X, dfloat _Y;
    _pfcopy(_X, _A);

    _pfmult(_A, _C);    IFNOT_ERR;
    _pfcopy(_Y, _B);
    _pfmult(_Y, _D);    IFNOT_ERR;
    _pfsub(_A, _Y);     IFNOT_ERR;      ;;; real part -> _A

    _pfmult(_B, _C);    IFNOT_ERR;
    _pfmult(_X, _D);    IFNOT_ERR;
    _pfadd(_B, _X);                     ;;; imag part -> _B
    return();

ERR:
    return(false)
enddefine;


    /*  ( _A * _C - _B * _D ) / E -> _A
        ( _B * _C - _A * _D ) / E -> _B
        where
            E = _C * _C + _D * _D
        but we compute it in a way that avoids intermediate overflow, i.e.
        by dividing everything thru by max(abs(_C), abs(_D)) first.
    */
define Cxf_div(_A, _B, _C, _D);
    lvars _A, _B, _C, _D, _MAX;
    lstackmem dfloat _MIN, dfloat _X, dfloat _Y;

    _pfcopy(_X, _C), _pfabs(_X);
    _pfcopy(_Y, _D), _pfabs(_Y);
    if _pfsgr(_Y, _X) then
        _pfcopy(_MIN, _C), _D -> _MAX;
    else
        _pfcopy(_MIN, _D), _C -> _MAX;
    endif;
    _pfdiv(_A, _MAX);       IFNOT_ERR;
    _pfdiv(_B, _MAX);       IFNOT_ERR;
    _pfdiv(_MIN, _MAX) -> ;

    _pfcopy(_X, _A);
    _pfcopy(_Y, _B);

    if _MAX == _D then
        _pfmult(_A, _MIN);  IFNOT_ERR;
        _pfmult(_B, _MIN);  IFNOT_ERR;
    else
        _pfmult(_X, _MIN);  IFNOT_ERR;
        _pfmult(_Y, _MIN);  IFNOT_ERR;
    endif;
    _pfadd(_A, _Y);         IFNOT_ERR;
    _pfsub(_B, _X);         IFNOT_ERR;

    _pfmult(_MIN, _MIN) -> ;
    _pfadd(_MIN, _1.0) -> ;     ;;; 1 + MIN**2
    return(_pfdiv(_A, _MIN) and _pfdiv(_B, _MIN));

ERR:
    return(false)
enddefine;

    /*  compute absolute value of complex _A +i _B into _abs, computed as
            MAX * sqrt(1 + (MIN/MAX)**2)
        where MAX = max(abs(A),abs(B)) and MIN = min(abs(B),abs(B))
    */
define Cxf_abs(_abs, _A, _B);
    lvars _A, _B, _abs;
    lstackmem dfloat _dmin, dfloat _dmax;
    lvars _MIN = _dmin, _MAX = _dmax;

    _pfcopy(_MAX, _A), _pfabs(_MAX);
    _pfcopy(_MIN, _B), _pfabs(_MIN);
    if _pfsgr(_MIN, _MAX) then _MAX, _MIN -> (_MIN, _MAX) endif;
    if _pfzero(_MAX) then
        ;;; both zero
        _pfcopy(_abs, _MAX);
        true
    else
        _pfdiv(_MIN, _MAX) -> ;         ;;; MIN/MAX
        _pfmult(_MIN, _MIN) -> ;        ;;; squared
        _pfadd(_MIN, _1.0) -> ;         ;;; + 1
        _MATH1(_MIN, _f_SQRT) -> ;      ;;; sqrt(1+(MIN/MAX)**2)
        _pfcopy(_abs, _MIN);
        _pfmult(_abs, _MAX)             ;;; return its result
    endif
enddefine;

define Cxf_arith_2(x, y, cxf_op);
    lvars x, y, procedure cxf_op, _A = _dfresult, _B = _dfradix,
        _C = _dfop1, _D = _dfop2, _argtype;
    Dfloat(x!CX_REAL, _A) _biset Dfloat(y!CX_REAL, _C) -> _argtype;
    Dfloat(x!CX_IMAG, _B) -> ;
    Dfloat(y!CX_IMAG, _D) -> ;
    if cxf_op(_A, _B, _C, _D) then
        Get_df_complex(_argtype, _A, _B)
    else
        Float_overflow(x, y, 2, false, false)
    endif
enddefine;

    /*  call cxf_op on complex x, realpart into _dfop1 and imag part in _dfop2
    */
define Cxf_op_cmplx(x, cxf_op);
    lvars x, procedure cxf_op, _A = _dfop1, _B = _dfop2, _argtype;
    Dfloat(x!CX_REAL, _A) -> _argtype;
    Dfloat(x!CX_IMAG, _B) -> ;
    if cxf_op(_A, _B) then
        Get_df_complex(_argtype, _A, _B), true
    else
        false
    endif
enddefine;

    /*  call cxf_op on real x in _dfop1 with 0 imag part in _dfop2
    */
define Cxf_op_real(_argtype, cxf_op);
    lvars procedure cxf_op, _A = _dfop1, _B = _dfop2, _argtype;
    _pfcopy(_B, _0.0);
    if cxf_op(_A, _B) then
        Get_df_complex(_argtype, _A, _B), true
    else
        false
    endif
enddefine;

define lconstant Cx_float_abs(x) -> _argtype;
    lvars x, _argtype;
    ;;; get real into _dfop1, imag into _dfop2, and abs(x) into _dfresult
    Dfloat(x!CX_REAL, _dfop1) -> _argtype;
    Dfloat(x!CX_IMAG, _dfop2) -> ;
    unless Cxf_abs(_dfresult, _dfop1, _dfop2) then
        Float_overflow(x, 1, 'computing abs of complex', 'arith-abs-complex')
    endunless
enddefine;

    ;;; absolute value of complex x
define Complex_abs() with_nargs 1;
    Consdecimal(Cx_float_abs(), _dfresult)
enddefine;

    ;;; sign of complex x
define Complex_sign(x);
    lvars x, _real = _dfop1, _imag = _dfop2, _abs = _dfresult, _argtype;
    Cx_float_abs(x) -> _argtype;
    if _pfzero(_abs) then return(x) endif;
    _pfdiv(_real, _abs) -> ;
    _pfdiv(_imag, _abs) -> ;
    Get_df_complex(_argtype, _real, _imag)
enddefine;

define Cxf_is_float_real(complex, _df_opnd);
    lvars complex, i, _df_opnd;
    complex!CX_IMAG -> i;
    if issimple(i) then
        if isinteger(i) then
            return(false)
        else
            _pf_dfloat_dec(i, _df_opnd)
        endif
    elseif i!KEY /== ddecimal_key then
        return(false)
    else
        _pf_dfloat_ddec(i, _df_opnd)
    endif;
    if _pfzero(_df_opnd) then
        ;;; float the real part
        complex!CX_REAL -> i;
        if issimple(i) then
            _pf_dfloat_dec(i, _df_opnd)
        else
            _pf_dfloat_ddec(i, _df_opnd)
        endif;
        true
    else
        false
    endif
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 25 1995
        Replaced _m*ath1 subroutine with _MATH1 macro
--- John Gibson, Jun  2 1994
        Replaced work floats with lstackmem
 */
