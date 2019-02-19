/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/src/complex_power.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;---------------------- COMPLEX ** ---------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'mathlib.ph'

constant
        procedure (Sys$-Cxf_log)
    ;

;;; ----------------------------------------------------------------------

section $-Sys;

    /* ** for an integral power
    */
define Cxf_**_int_power(power, _was_+, _A, _B);
    lvars power, _A, _B, _was_+;
    lstackmem dfloat _X, dfloat _Y, dfloat _U, dfloat _V;

    Cxf_copy(_X, _Y, _1.0, _0.0);
    repeat
        if testbit(power, 0) then
            Cxf_mult(_X, _Y, _A, _B); IFNOT_ERR;
        endif;
        quitif((power >> 1 ->> power) == 0);
        Cxf_copy(_U, _V, _A, _B);
        Cxf_mult(_A, _B, _U, _V);   IFNOT_ERR;
    endrepeat;
    if _was_+ then
        Cxf_copy(_A, _B, _X, _Y)
    else
        Cxf_copy(_A, _B, _1.0, _0.0);
        Cxf_div(_A, _B, _X, _Y);    IFNOT_ERR;
    endif;
    return(true);

ERR:
    return(false)
enddefine;

    /*  ** for a non-integral or complex power.
        power is floated in _dfresult, _dfradix
    */
define Cxf_**(_A, _B);
    lvars _A, _B;
    if Cxf_log(_A, _B) then
        ;;; base /= 0
        unless Cxf_mult(_A, _B, _dfresult, _dfradix) and Cxf_exp(_A, _B) then
            return(false)
        endunless
    else
        ;;; base = 0, error unless real part of power _dfresult > 0
        if _pfzero(_dfresult) or _pfneg(_dfresult) then
            return(false)
        endif
    endif;
    true
enddefine;


endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun  2 1994
        Replaced work floats with lstackmem
--- John Gibson, Dec  6 1987
        Tidied up.
 */
