/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/src/complex_invtrig.p
 > Purpose:
 > Author:          John Gibson 1988 (see revisions)
 */

;;;---------------- COMPLEX INVERSE TRIG FUNCTIONS ----------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'mathlib.ph'


constant
        Sys$- _log_2
    ;

;;; ------------------------------------------------------------------------

section $-Sys;

    /*  produce -i * cxf_op(i*(_A+i_B))
    */
define lconstant -:_op_+:(_A, _B, cxf_op);
    lvars procedure cxf_op, _A, _B;
    _pfnegate(_B);
    cxf_op(_B, _A);         ;;; swop real, imag
    _pfnegate(_B);
enddefine;


    /*  Do op(_A, _B) after transforming to Quadrant I, transforming
        back result -- for odd functions arcsin(h) and arctan(h)
    */
define lconstant QI_op(_A, _B, cxf_op);
    lvars procedure cxf_op, _A, _B, _negated, _conjugated;
    if _pfzero(_A) then
        false -> _conjugated;
        if _pfneg(_B) ->> _negated then _pfnegate(_B) endif
    else
        if _pfneg(_A) ->> _negated then
            _pfnegate(_A), _pfnegate(_B);   ;;; negate it
        endif;
        if _pfneg(_B) ->> _conjugated then
            _pfnegate(_B)                   ;;; conjugate it
        endif
    endif;
    cxf_op(_A, _B);         ;;; now _A >= 0, _B >= 0
    if _conjugated then
        _pfnegate(_B)                   ;;; un-conjugate
    endif;
    if _negated then
        _pfnegate(_A), _pfnegate(_B)    ;;; un-negate
    endif;
enddefine;

    /*  arcsinh(z) for z in Quadrant I = log(z + sqrt(1 + z*z))
    */
define lconstant Arcsinh(_A, _B);
    lvars _A, _B;
    lstackmem dfloat _X, dfloat _Y;
    Cxf_copy(_X, _Y, _A, _B);
    if Cxf_mult(_X, _Y, _A, _B) and _pfadd(_X, _1.0) then
        Cxf_sqrt(_X, _Y) -> ;
        Cxf_add(_A, _B, _X, _Y) -> ;
        Cxf_log(_A, _B)
    else
        ;;; else it's log(2z) = log(z) + log(2)
        Cxf_log(_A, _B) -> ;
        _pfadd(_A, _log_2)
    endif
enddefine;

    /*  arctanh(z) for z in Quadrant I = log( (1+z)/(1-z) ) / 2
    */
define lconstant Arctanh(_A, _B);
    lvars _A, _B;
    lstackmem dfloat _X, dfloat _Y;
    Cxf_copy(_X, _Y, _A, _B);
    _pfnegate(_X), _pfnegate(_Y);
    if _pfadd(_X, _1.0) and _pfadd(_A, _1.0) then
        unless Cxf_div(_A, _B, _X, _Y) and Cxf_log(_A, _B) then
            ;;; arg to near +1 or -1, overflows
            return(false)
        endunless
    else
        ;;; too big -- result is log(-1)/2
        Cxf_copy(_A, _B, _-1.0, _0.0);
        Cxf_log(_A, _B) ->
    endif;
    _pfmult(_A, _0.5) ->, _pfmult(_B, _0.5)
enddefine;

lconstant procedure (
    Cxf_arcsinh = QI_op(%Arcsinh%),
    Cxf_arctanh = QI_op(%Arctanh%),

    Cxf_arcsin  = -:_op_+:(%Cxf_arcsinh%),  /*  arcsin(z) = -i arcsinh(iz) */
    Cxf_arctan  = -:_op_+:(%Cxf_arctanh%),  /*  arctan(z) = -i arctanh(iz) */
    );


    /*  arccos(z) = pi/2 - arcsin(z)
    */
define lconstant Cxf_arccos(_A, _B);
    lvars _A, _B;
    Cxf_arcsin(_A, _B);
    _pfnegate(_A), _pfnegate(_B);
    _pfadd(_A, _1.570796326794897) ->       ;;; + pi/2
enddefine;

    /*  arccosh(z) = log(z + (z+1)sqrt( (z-1)/(z+1) ))
    */
define lconstant Cxf_arccosh(_A, _B);
    lvars large, _A, _B;
    lstackmem dfloat _X, dfloat _Y;
    Cxf_copy(_X, _Y, _A, _B);
    _pfsub(_X, _1.0) -> ;
    _pfadd(_A, _1.0) -> ;
    false -> large;
    if Cxf_div(_X, _Y, _A, _B) then
        Cxf_sqrt(_X, _Y) -> ;
        if Cxf_mult(_X, _Y, _A, _B) and Cxf_add(_X, _Y, _A, _B) then
            Cxf_copy(_A, _B, _X, _Y)
        else
            ;;; result is log(2*z) for large z
            true -> large
        endif
    ;;; else z is near -1, result is log(z)
    endif;
    _pfsub(_A, _1.0) -> ;
    Cxf_log(_A, _B);
    if large then _pfadd(_A, _log_2) -> endif
enddefine;

lconstant
    Triginv_ops = initv(TRIGOP_VEC_LEN);

    Cxf_arcsin  -> subscrv(TRIGOP_sin, Triginv_ops),
    Cxf_arccos  -> subscrv(TRIGOP_cos, Triginv_ops),
    Cxf_arctan  -> subscrv(TRIGOP_tan, Triginv_ops),
    Cxf_arcsinh -> subscrv(TRIGOP_sinh, Triginv_ops),
    Cxf_arccosh -> subscrv(TRIGOP_cosh, Triginv_ops),
    Cxf_arctanh -> subscrv(TRIGOP_tanh, Triginv_ops),
    ;


    /*  call operation from vector on real and imag parts of complex
        converting result to degrees if necessary
    */
define Cxf_trigop_inv(opsub, _A, _B);
    lvars opsub, _A, _B;
    fast_apply(_A, _B, fast_subscrv(opsub, Triginv_ops));
    unless popradians then
        () and _pfdiv(_A, _pi_/_180) and _pfdiv(_B, _pi_/_180)
    endunless
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun  2 1994
        Replaced work dfloats with lstackmem
--- John Gibson, Dec  6 1987
        Tidied up
 */
