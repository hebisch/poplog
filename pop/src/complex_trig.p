/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/complex_trig.p
 > Purpose:
 > Author:          John Gibson 1988 (see revisions)
 */

;;;------------------ COMPLEX TRIG FUNCTIONS ------------------------------
;;;             all called with real and imaginary parts _A, _B
;;;                 false return on overflow

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'mathlib.ph'

;;; -----------------------------------------------------------------------

section $-Sys;


;;; --- CIRCULAR ---------------------------------------------------------

    /*  sin(_A + i_B) = sin(_A)cosh(_B) + i cos(_A)sinh(_B)
    */
define lconstant Cxf_sin(_A, _B);
    lvars _A, _B;
    lstackmem dfloat _X, dfloat _Y;
    Cxf_copy(_X, _Y, _A, _B);
    unless _MATH1(_Y, _f_COSH) and _MATH1(_B, _f_SINH) then
        return(false)
    endunless;
    _MATH1(_A, _f_SIN) -> ;
    _pfmult(_A, _Y) -> ;        ;;; sin(_A)cosh(_B)
    _MATH1(_X, _f_COS) -> ;
    _pfmult(_B, _X)             ;;; cos(_A)sinh(_B)
enddefine;

    /*  cos(_A + iB) = cos(_A)cosh(_B) - i sin(_A)sinh(_B)
    */
define lconstant Cxf_cos(_A, _B);
    lvars _A, _B;
    lstackmem dfloat _X, dfloat _Y;
    Cxf_copy(_X, _Y, _A, _B);
    returnunless(_MATH1(_Y, _f_COSH) and _MATH1(_B, _f_SINH)) (false);
    _MATH1(_A, _f_COS) -> ;
    _pfmult(_A, _Y) -> ;        ;;; cos(_A)cosh(_B)
    _MATH1(_X, _f_SIN) -> ;
    _pfnegate(_X);
    _pfmult(_B, _X)             ;;; -sin(_A)sinh(_B)
enddefine;


    /*  tan (_A + i_B) =

            cos(_A)sin(_A)(1-tanh(_B)**2) + i tanh(_B)
            -------------------------------------
              cos(_A)**2 + sin(_A)**2 tanh(_B)**2

        (compute it like this rather than with tan(_A), because this goes to
        infinity at pi/2 for the real case only).
    */
define lconstant Cxf_tan(_CA, _THB);        ;;; actually, _A and _B
    lvars _CA, _THB;
    lstackmem dfloat _SA, dfloat _THB_sq, dfloat _D;

    _pfcopy(_SA, _CA);
    _MATH1(_SA, _f_SIN) -> ;        ;;; sin(_A)
    _MATH1(_CA, _f_COS) -> ;        ;;; cos(_A)
    _MATH1(_THB, _f_TANH) -> ;      ;;; tanh(_B)
    _pfcopy(_THB_sq, _THB);
    _pfmult(_THB_sq, _THB) -> ;     ;;; tanh(_B)**2
    ;;; denominator
    _pfcopy(_D, _CA);
    _pfmult(_D, _CA) -> ;           ;;; cos(_A)**2
    _pfmult(_CA, _SA) -> ;          ;;; cos(_A)sin(_A) into real part
    _pfmult(_SA, _SA) -> ;          ;;; sin(_A)**2
    _pfmult(_SA, _THB_sq) -> ;      ;;; sin(_A)**2 tanh(_B)**2
    _pfadd(_D, _SA) -> ;            ;;; + cos(_A)**2
    ;;; finish real part
    _pfnegate(_THB_sq);
    _pfadd(_THB_sq, _1.0) -> ;      ;;; 1 - tanh(_B)**2
    _pfmult(_CA, _THB_sq) -> ;
    ;;; divide real part and imag part
    _pfdiv(_CA, _D) and _pfdiv(_THB, _D)
enddefine;


;;; --- HYPERBOLIC ---------------------------------------------------------

    /*  produce -i * cxf_op(i*(_A+i_B))
    */
define lconstant -:_op_+:(_A, _B, cxf_op);
    lvars procedure cxf_op, _A, _B;
    _pfnegate(_B);
    cxf_op(_B, _A);         ;;; swop real, imag
    _pfnegate(_B)
enddefine;

lconstant procedure (
    Cxf_sinh    = -:_op_+:(%Cxf_sin%),  /*  sinh(z) = -i sin(iz) */
    Cxf_tanh    = -:_op_+:(%Cxf_tan%),  /*  tanh(z) = -i tan(iz) */
    );

    /* cosh(z) = cos(iz)
    */
define lconstant Cxf_cosh(_A, _B);
    lvars _A, _B;
    lstackmem dfloat _X;
    _pfcopy(_X, _A), _pfcopy(_A, _B), _pfcopy(_B, _X);  ;;; swop _A, _B
    _pfnegate(_A);
    Cxf_cos(_A, _B)
enddefine;

lconstant
    Trig_ops = initv(TRIGOP_VEC_LEN);

    Cxf_sin     -> subscrv(TRIGOP_sin, Trig_ops),
    Cxf_cos     -> subscrv(TRIGOP_cos, Trig_ops),
    Cxf_tan     -> subscrv(TRIGOP_tan, Trig_ops),
    Cxf_sinh    -> subscrv(TRIGOP_sinh, Trig_ops),
    Cxf_cosh    -> subscrv(TRIGOP_cosh, Trig_ops),
    Cxf_tanh    -> subscrv(TRIGOP_tanh, Trig_ops),
    ;


    /*  call operation from vector on real and imag parts of complex
        converted from degrees if necessary
    */
define Cxf_trigop(opsub, _A, _B);
    lvars opsub, _A, _B;
    unless popradians then
        _pfmult(_A, _pi_/_180) -> , _pfmult(_B, _pi_/_180) ->
    endunless;
    fast_apply(_A, _B, fast_subscrv(opsub, Trig_ops))
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 25 1995
        Replaced _m*ath1 subroutine with _MATH1 macro
--- John Gibson, Jun  2 1994
        Replaced work floats with lstackmem
 */
