/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/complex_exp.p
 > Purpose:
 > Author:          John Gibson 1988 (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;; ---------------- COMPLEX EXPONENT & LOG -----------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'mathlib.ph'


section $-Sys;

    /*  log of complex x = log(abs(x)) + i*phase(x)
        error return (false) if x = 0
    */
define Cxf_log(_A, _B);
    lvars _A, _B;
    lstackmem dfloat _w1, dfloat _w2;
    lvars _MIN = _w1, _MAX = _w2;

    Cxf_copy(_MAX, _MIN, _A, _B);
    _pfabs(_MAX), _pfabs(_MIN);
    if _pfsgr(_MIN, _MAX) then
        _MAX, _MIN -> _MAX -> _MIN
    endif;
    ;;; return false if both zero
    returnunless(_pfdiv(_MIN, _MAX)) (false);       ;;; MIN/MAX
    _MATH2(_B, _A, _f_ATAN2) -> ;   ;;; arctan(_B/_A) into _B
    _pfmult(_MIN, _MIN) -> ;        ;;; squared
    _pfadd(_MIN, _1.0) -> ;         ;;; + 1
    _MATH1(_MIN, _f_SQRT) -> ;      ;;; sqrt(1+(MIN/MAX)**2)
    _pfcopy(_A, _MIN);
    if _pfmult(_A, _MAX) then
        _MATH1(_A, _f_LOG)
    else
        _pfcopy(_A, _MIN);
        _MATH1(_A, _f_LOG) -> ;
        _MATH1(_MAX, _f_LOG) -> ;
        _pfadd(_A, _MAX)
    endif
enddefine;

    /*  exp of complex _A + i_B  is  exp(_A)*(cos(_B) + isin(_B))
        error return (false) if overflow
    */
define Cxf_exp(_A, _B);
    lvars _A, _B;
    lstackmem dfloat _exp;
    _pfcopy(_exp, _A);
    returnunless(_MATH1(_exp, _f_EXP)) (false);
    _pfcopy(_A, _B);
    _MATH1(_A, _f_COS) -> ;
    _pfmult(_A, _exp) -> ;
    _MATH1(_B, _f_SIN) -> ;
    _pfmult(_B, _exp)
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 25 1995
        Replaced _m*ath1/2 subroutine with _MATH1/2 macro
 */
