/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/src/arith2.p
 > Purpose:
 > Author:          John Gibson, Feb  3 1988 (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;; --------------- 2 ARG GENERIC ARITHMETIC ---------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

global constant
        _padd_testovf, _psub_testovf
    ;

section $-Sys;

constant
        procedure (Bigint_dfloat, Ratio_dfloat,
        Ratio_cons, Ratio_op_aint_ratio, Ratio_op_ratio_aint,
        Complex_op_cmplx_real, Complex_op_real_cmplx,
        )
        Ratio_ops, Float_ops, Complex_ops,
    ;

endsection;


;;; ---------------------------------------------------------------------

section $-Sys =>  +  -  *  / ;

lconstant
    rat_not_loaded_ms = 'INEXACT INTEGER DIVISION (ratios not loaded)';

define Int_overflow_op(x, y, _opsub);
    lvars x, y, _opsub;
    if testdef biginteger_key then
        chain(x, y, fast_subscrv(_opsub, BGWEAK Bigint_ops),
                                                BGWEAK Bigint_op_int_int)
    else
        mishap(x, y, 2, 'INTEGER OVERFLOW (bigintegers not loaded)')
    endif
enddefine;

    ;;; arithmetic operation of 2 arguments
define Arith_2(x, y, _opsub);
    lvars x, y, _type, _opsub;
    if issimple(x) then
        if isinteger(x) then
            _shift(_:NUMTYPE_INTEGER, _3) -> _type
        else
            _shift(_:NUMTYPE_DECIMAL, _3) -> _type
        endif
    else
        _shift(x!KEY!K_NUMBER_TYPE, _3) -> _type
    endif;
    if issimple(y) then
        if isinteger(y) then
            _:NUMTYPE_INTEGER _add _type -> _type
        else
            _:NUMTYPE_DECIMAL _add _type -> _type
        endif
    else
        y!KEY!K_NUMBER_TYPE _add _type -> _type
    endif;

    ;;; switch on 64 different type combinations
    ;;; types are: non-number int bigint ratio dec ddec complex  unused
    ;;;                0       1    2      3    4    5     6        7

    go_on _pint(_type) to

    ;;;   01          02          03          04          05          06          07          10
    ;;;   ?           ?           ?           ?           ?           ?           ?           ?
        ERROR       ERROR       ERROR       ERROR       ERROR       ERROR       ERROR       ERROR

    ;;;   11          12          13          14          15          16          17          20
    ;;; int-int     int-bgint   int-ratio   int-dec     int-ddec    int-cmplx     ?           ?
        INT_INT     INT_BGINT   AINT_RATIO  INT_DEC     INT_DDEC    RAT_CMPLX   ERROR       ERROR

    ;;;   21          22          23          24          25          26          27          30
    ;;; bgint-int   bgint-bgint bgint-ratio bgint-dec   bgint-ddec  bgint-cmplx   ?           ?
        BGINT_INT   DOBIGINT    AINT_RATIO  BGINT_DEC   BGINT_DDEC  RAT_CMPLX   ERROR       ERROR

    ;;;   31          32          33          34          35          36          37          40
    ;;; ratio-int   ratio-bgint ratio-ratio ratio-dec   ratio-ddec  ratio-cmplx   ?           ?
        RATIO_AINT  RATIO_AINT  DORATIO     RATIO_DEC   RATIO_DDEC  RAT_CMPLX   ERROR       ERROR

    ;;;   41          42          43          44          45          46          47          50
    ;;; dec-int     dec-bgint   dec-ratio   dec-dec     dec-ddec    dec-cmplx     ?           ?
        DEC_INT     DEC_BGINT   DEC_RATIO   DEC_DEC     DEC_DDEC    DEC_CMPLX   ERROR       ERROR

    ;;;   51          52          53          54          55          56          57          60
    ;;; ddec-int    ddec-bgint  ddec-ratio  ddec-dec    ddec-ddec   ddec-cmplx    ?           ?
        DDEC_INT    DDEC_BGINT  DDEC_RATIO  DDEC_DEC    DDEC_DDEC   DDEC_CMPLX  ERROR       ERROR

    ;;;   61          62          63          64          65          66          67          70
    ;;; cmplx-int   cmplx-bgint cmplx-ratio cmplx-dec   cmplx-ddec  cmplx-cmplx   ?           ?
        CMPLX_RAT   CMPLX_RAT   CMPLX_RAT   CMPLX_DEC   CMPLX_DDEC  DOCMPLX     ERROR       ERROR

    else ERROR;

    INT_INT:
        chain(x, y, _opsub, Int_overflow_op);

    INT_BGINT:
        if fast_subscrv(_opsub, BGWEAK Bigint_ops) ->> _type then
            chain(x, y, _type, BGWEAK Bigint_op_int_bgint)
        endif;

        ;;; op vec contains false for /
    try_cons_ratio:
        if testdef ratio_key then
            chain(x, y, RTWEAK Ratio_cons)
        else
            mishap(x, y, 2, rat_not_loaded_ms)
        endif;

    AINT_RATIO:
        chain(x, y, fast_subscrv(_opsub, RTWEAK Ratio_ops),
                                            RTWEAK Ratio_op_aint_ratio);

    INT_DEC:
        FLWEAK _pf_dfloat_int(_int(x), FLWEAK _dfop1);
        FLWEAK _pf_dfloat_dec(y, FLWEAK _dfop2);
        _:ARGTYPE_RATIONAL _biset _:ARGTYPE_DECIMAL -> _type;
        goto DODDEC;

    INT_DDEC:
        FLWEAK _pf_dfloat_int(_int(x), FLWEAK _dfop1);
        FLWEAK _pf_dfloat_ddec(y, FLWEAK _dfop2);
        _:ARGTYPE_RATIONAL _biset _:ARGTYPE_DDECIMAL -> _type;
        goto DODDEC;

    RAT_CMPLX:
        0 -> _type;  goto REAL_CMPLX;

    BGINT_INT:
        if fast_subscrv(_opsub, BGWEAK Bigint_ops) ->> _type then
            chain(x, y, _type, BGWEAK Bigint_op_bgint_int)
        endif;

        ;;; op vec contains false for /
        goto try_cons_ratio;

    RATIO_AINT:
        chain(x, y, fast_subscrv(_opsub, RTWEAK Ratio_ops),
                                            RTWEAK Ratio_op_ratio_aint);

    BGINT_DEC:
        BGWEAK Bigint_dfloat(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_dec(y, FLWEAK _dfop2);
        _:ARGTYPE_RATIONAL _biset _:ARGTYPE_DECIMAL -> _type;
        goto DODDEC;

    BGINT_DDEC:
        BGWEAK Bigint_dfloat(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_ddec(y, FLWEAK _dfop2);
        _:ARGTYPE_RATIONAL _biset _:ARGTYPE_DDECIMAL -> _type;
        goto DODDEC;

    RATIO_DEC:
        RTWEAK Ratio_dfloat(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_dec(y, FLWEAK _dfop2);
        _:ARGTYPE_RATIONAL _biset _:ARGTYPE_DECIMAL -> _type;
        goto DODDEC;

    RATIO_DDEC:
        RTWEAK Ratio_dfloat(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_ddec(y, FLWEAK _dfop2);
        _:ARGTYPE_RATIONAL _biset _:ARGTYPE_DDECIMAL -> _type;
        goto DODDEC;

    DEC_INT:
        FLWEAK _pf_dfloat_dec(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_int(_int(y), FLWEAK _dfop2);
        _:ARGTYPE_RATIONAL _biset _:ARGTYPE_DECIMAL -> _type;
        goto DODDEC;

    DEC_BGINT:
        FLWEAK _pf_dfloat_dec(x, FLWEAK _dfop1);
        BGWEAK Bigint_dfloat(y, FLWEAK _dfop2);
        _:ARGTYPE_RATIONAL _biset _:ARGTYPE_DECIMAL -> _type;
        goto DODDEC;

    DEC_RATIO:
        FLWEAK _pf_dfloat_dec(x, FLWEAK _dfop1);
        RTWEAK Ratio_dfloat(y, FLWEAK _dfop2);
        _:ARGTYPE_RATIONAL _biset _:ARGTYPE_DECIMAL -> _type;
        goto DODDEC;

    DEC_DEC:
        FLWEAK _pf_dfloat_dec(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_dec(y, FLWEAK _dfop2);
        _:ARGTYPE_DECIMAL -> _type;
        goto DODDEC;

    DEC_DDEC:
        FLWEAK _pf_dfloat_dec(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_ddec(y, FLWEAK _dfop2);
        _:ARGTYPE_DECIMAL _biset _:ARGTYPE_DDECIMAL -> _type;
        goto DODDEC;

    DEC_CMPLX:
        0.0s0 -> _type; goto REAL_CMPLX;

    DDEC_INT:
        FLWEAK _pf_dfloat_ddec(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_int(_int(y), FLWEAK _dfop2);
        _:ARGTYPE_RATIONAL _biset _:ARGTYPE_DDECIMAL -> _type;
        goto DODDEC;

    DDEC_BGINT:
        FLWEAK _pf_dfloat_ddec(x, FLWEAK _dfop1);
        BGWEAK Bigint_dfloat(y, FLWEAK _dfop2);
        _:ARGTYPE_RATIONAL _biset _:ARGTYPE_DDECIMAL -> _type;
        goto DODDEC;

    DDEC_RATIO:
        FLWEAK _pf_dfloat_ddec(x, FLWEAK _dfop1);
        RTWEAK Ratio_dfloat(y, FLWEAK _dfop2);
        _:ARGTYPE_RATIONAL _biset _:ARGTYPE_DDECIMAL -> _type;
        goto DODDEC;

    DDEC_DEC:
        FLWEAK _pf_dfloat_ddec(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_dec(y, FLWEAK _dfop2);
        _:ARGTYPE_DECIMAL _biset _:ARGTYPE_DDECIMAL -> _type;
        goto DODDEC;

    DDEC_DDEC:
        FLWEAK _pf_dfloat_ddec(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_ddec(y, FLWEAK _dfop2);
        _:ARGTYPE_DDECIMAL -> _type;
        goto DODDEC;

    DDEC_CMPLX:
        FLWEAK double_float_0 -> _type;
        ;;; drop thru to REAL_CMPLX

    REAL_CMPLX:
        ;;; _type contains 0 of the appropriate type for imag part
        chain(x, _type, y, fast_subscrv(_opsub, CXWEAK Complex_ops),
                                            CXWEAK Complex_op_real_cmplx);

    CMPLX_RAT:
        0 -> _type;  goto CMPLX_REAL;

    CMPLX_DEC:
        0.0s0 -> _type;  goto CMPLX_REAL;

    CMPLX_DDEC:
        FLWEAK double_float_0 -> _type;
        ;;; drop thru to CMPLX_REAL

    CMPLX_REAL:
        ;;; _type contains 0 of the appropriate type for imag part
        chain(x, y, _type, fast_subscrv(_opsub, CXWEAK Complex_ops),
                                            CXWEAK Complex_op_cmplx_real);


    DOBIGINT:
        _CHECKINTERRUPT;
        if fast_subscrv(_opsub, BGWEAK Bigint_ops) ->> _type then
            fast_chain(x, y, _type)
        endif;
        ;;; op vec contains false for /
        goto try_cons_ratio;

    DORATIO:
        _CHECKINTERRUPT;
        fast_chain(x, y, fast_subscrv(_opsub, RTWEAK Ratio_ops));

    DODDEC:
        ;;; operation is a subroutine returning false for overflow
        fast_subscrv(_opsub, FLWEAK Float_ops) -> _opsub;
        unless _opsub(_type, FLWEAK _dfop1, FLWEAK _dfop2) then
            -> ;                ;;; erase _type
            FLWEAK Float_overflow(x, y, 2, false, false)
        else
            ;;; result is in _dfop1 (_type is on the stack)
            chain(FLWEAK _dfop1, FLWEAK Consdecimal)
        endunless;

    DOCMPLX:
        fast_chain(x, y, fast_subscrv(_opsub, CXWEAK Complex_ops));

    ERROR:
        mishap(x, y, 2, 'NUMBER(S) NEEDED', 'arith-2arg:type-number')
enddefine;      /* Arith_2 */


define 5 x + y;
    lvars x, y;
    if isinteger(x) and isinteger(y) then
        if _padd_testovf(x, y) then return() else -> endif
    endif;
    Arith_2(x, y, OP_+)
enddefine;

define +_1(x) with_props +;
    lvars x;
    if isinteger(x) then
        if _padd_testovf(x, 1) then return() else -> endif
    endif;
    Arith_2(x, 1, OP_+)
enddefine;

define 5 x - y;
    lvars x, y;
    if isinteger(x) and isinteger(y) then
        if _psub_testovf(x, y) then return() else -> endif
    endif;
    Arith_2(x, y, OP_-)
enddefine;

define -_1(x) with_props -;
    lvars x;
    if isinteger(x) then
        if _psub_testovf(x, 1) then return() else -> endif
    endif;
    Arith_2(x, 1, OP_-)
enddefine;

define 4 x * y;
    lvars x y;
    if isinteger(x) and isinteger(y) then
        if _pmult_testovf(x, y) then return else -> endif
    endif;
    Arith_2(x, y, OP_*)
enddefine;

define 4 x / y;
    lvars x, y, _res;
    if isinteger(y) then
        if y == 0 then
            mishap(x, y, 2, 'DIVIDING BY ZERO', 'arith-2arg:arith-div0')
        elseif isinteger(x) then
            if y == -1 then
                ;;; dividing largest -ve integer by -1 can overflow!
                return(negate(x))
            elseif (x fi_// y -> _res) == 0 then
                ;;; divides exactly
                return(_res)
            elseif testdef ratio_key then
                return(RTWEAK Ratio_cons(x, y))
            else
                mishap(x, y, 2, rat_not_loaded_ms)
            endif
        endif
    endif;
    Arith_2(x, y, OP_/)
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 10 1998
        Replaced single*_float_0 with 0.0s0
--- John Gibson, Apr  1 1996
        Added some mishap id-strings
--- John Gibson, Aug 12 1988
        Used chaining of procedure where possible
--- John Gibson, Jul 16 1988
        Corrected erroneous result from dividing largest -ve popint by -1
 */
