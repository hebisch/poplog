/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/arith1.p
 > Purpose:
 > Author:          John Gibson, Feb  3 1988 (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;; --------------- 1 ARG GENERIC ARITHMETIC ---------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

global constant
        _psub_testovf
    ;

section $-Sys;

constant
        Ratio_ops, Float_ops, Complex_ops,
    ;

endsection;


;;; -----------------------------------------------------------------------

section $-Sys => negate abs intof fracof round sign;

define lconstant Arith_1(x, _opsub);
    lvars x, _opsub;
    if issimple(x) then
        FLWEAK _pf_dfloat_dec(x, FLWEAK _dfop1);
        _:ARGTYPE_DECIMAL;
    do_float:
        fast_chain((), FLWEAK _dfop1, fast_subscrv(_opsub, FLWEAK Float_ops))
    endif;

    go_on _pint(x!KEY!K_NUMBER_TYPE) to
        ;;;   1       2       3       4       5        6
            ERROR   BIGINT  RATIO    ERROR  DDEC    COMPLEX
    else ERROR;

    BIGINT:
        fast_chain(x, fast_subscrv(_opsub, BGWEAK Bigint_ops));

    RATIO:
        fast_chain(x, fast_subscrv(_opsub, RTWEAK Ratio_ops));

    DDEC:
        FLWEAK _pf_dfloat_ddec(x, FLWEAK _dfop1);
        _:ARGTYPE_DDECIMAL;
        goto do_float;

    COMPLEX:
        fast_chain(x, fast_subscrv(_opsub, CXWEAK Complex_ops));

    ERROR:
        mishap(x, 1, 'NUMBER NEEDED', 'arith-1arg:type-number')
enddefine;      /* Arith_1 */

define negate(x);
    lvars x;
    if isinteger(x) then
        if _psub_testovf(0, x) then return else ->, 0 - x endif;
    else
        Arith_1(x, OP_negate)
    endif
enddefine;

define abs(x);
    lvars x;
    if isinteger(x) then
        if x fi_< 0 then
            if _psub_testovf(0, x) then return else ->, 0 - x endif
        else
            x
        endif
    else
        Arith_1(x, OP_abs)
    endif
enddefine;

define intof(x);
    lvars x;
    if isinteger(x) then x else Arith_1(x, OP_intof) endif
enddefine;

define fracof(x);
    lvars x;
    if isinteger(x) then 0 else Arith_1(x, OP_fracof) endif
enddefine;

define round(x);
    lvars x;
    if isinteger(x) then x else Arith_1(x, OP_round) endif
enddefine;

define sign(x);
    lvars x;
    if isinteger(x) then
        if x fi_> 0 then 1 elseif x == 0 then 0 else -1 endif
    else
        Arith_1(x, OP_sign)
    endif
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 12 1988
        Used chaining of procedures where possible
 */
