/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/complex_sqrt.p
 > Purpose:
 > Author:          John Gibson 1988 (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;; ------------------ COMPLEX SQUARE ROOT ----------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'mathlib.ph'

constant
        procedure (Sys$-Cxf_abs)
    ;

;;; ------------------------------------------------------------------------

section $-Sys;

    /*  square root of complex x = _A + i_B is
                _A >= 0:
                    r + i(_B/2r),   where r = sqrt(abs(x)/2 + _A/2)
                _A  < 0:
                    (_B/2r) + ir,   where r = sqrt(abs(x)/2 - _A/2),
                                                    negated if _B < 0
        no error return
    */
define Cxf_sqrt(_A, _B);
    lvars realneg, _A, _B;
    lstackmem dfloat _abs;

    if (_pfneg(_A) ->> realneg) then _pfnegate(_A) endif;
    ;;; halve both first
    _pfmult(_A, _0.5) -> ;
    _pfmult(_B, _0.5) -> ;
    ;;; get abs(_A/2+i_B/2) into _abs
    Cxf_abs(_abs, _A, _B)  -> ;     ;;; can't overflow
    returnif(_pfzero(_abs)) (true);
    _pfadd(_abs, _A) -> ;
    _MATH1(_abs, _f_SQRT) -> ;
    if realneg then
        if _pfneg(_B) then _pfnegate(_abs) endif;
        _pfcopy(_A, _B), _A, _B -> _A -> _B;
    endif;
    _pfcopy(_A, _abs);
    _pfdiv(_B, _abs)
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 25 1995
        Replaced _m*ath1 subroutine with _MATH1 macro
 */
