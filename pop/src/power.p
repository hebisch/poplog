/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/power.p
 > Purpose:
 > Author:          John Gibson 1988 (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;; ------------------------ ** OPERATOR -------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

global constant
        procedure (destcomplex, Sys$-Cxf_**_int_power,
        Sys$- **_frac_power, Sys$- **_float_base
        )
    ;

global vars
        pop_reduce_ratios
    ;

;;; -----------------------------------------------------------------------

section $-Sys =>  ** ;

    /*  ** for an integral power */
define lconstant **_int_power(base, power);
    lvars base, power, result, _btype, _was_+;
    if power == 0 then
        return(if base = 0 then base+1 else base/base endif, true)
    elseif power > 0 then
        true -> _was_+
    else
        negate(power) -> power;
        false -> _was_+
    endif;
    if issimple(base) then
        if isinteger(base) then
            goto RAT_BASE
        else
            goto DEC
        endif
    else
        go_on _pint(base!KEY!K_NUMBER_TYPE) to
            ;;;   1       2        3       4     5      6
                 ERR  RAT_BASE  RAT_BASE  ERR   DEC  COMPLEX
        else ERR;

        DEC:
            return(FLWEAK **_float_base(base, power, _was_+));

        COMPLEX:
            if testdef decimal_key and FLWEAK isdecimal(base!CX_REAL) then
                return(CXWEAK Cxf_op_cmplx(power, _was_+, base,
                                    weakref[Cxf_op_cmplx] Cxf_**_int_power))
            endif;
            goto RAT_BASE;

        ERR:
            return(false)
    endif;

RAT_BASE:
    ;;; powers of a ratio can't reduce
    dlocal RTWEAK pop_reduce_ratios = false;

    1 -> result;
    repeat
        if testbit(power, 0) then result*base -> result endif;
        quitif((power >> 1 ->> power) == 0);
        base * base -> base;
    endrepeat;
    unless _was_+ then
        returnif(result == 0) (false);
        ;;; make this true so that if result is -ve, sign gets transferred
        ;;; to numerator
        true -> weakref[ratio_key] pop_reduce_ratios;
        1 / result -> result
    endunless;
    return(result, true)
enddefine;

define 3 base ** power;
    lvars base, power, _result;
    _CLAWBACK_SAVE;
    if isintegral(power) then
        **_int_power(base, power)
    elseif testdef decimal_key then
        FLWEAK **_frac_power(base, power)
    else
        mishap(power, 1, 'INTEGER POWER NEEDED IN ** (floating-point not loaded)')
    endif;
    returnif() (Clawback_num());

    ;;; else analyse error
    unless isnumber(base) and isnumber(power) then
        mishap(base, power, 2, 'NUMBER(S) NEEDED')
    elseif base = 0 then
        mishap(base, power, 2, 'INVALID POWER FOR 0 BASE IN **')
    else
        FLWEAK Float_overflow(base, power, 2, false, 'arith-power')
    endunless
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 21 1988
        Corrected bug in **_int_power which caused it to produce a
        ratio with a negative denominator when power was negative.
--- John Gibson, Aug 16 1987
        Lconstant'ed, etc
 */
