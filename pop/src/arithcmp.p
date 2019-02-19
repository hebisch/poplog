/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/src/arithcmp.p
 > Purpose:
 > Author:          John Gibson, Feb  3 1988 (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;; ------------- GENERIC ARITHMETIC COMPARISON ------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

section $-Sys;

constant    procedure (Bigint_dfloat, Bigint_>, Ratio_dfloat, Ratio_>,
            Ratio_op_aint_ratio, Ratio_op_ratio_aint
            )
    ;

endsection;


;;; ----------------------------------------------------------------------

section $-Sys =>  >,  >=,  <,  <=,  min,  max, =<;

    ;;; x > (or =) y
define lconstant Arith_>(x, y, _or_=);
    lvars x, y, _type, _or_=;
    if issimple(x) then
        if isinteger(x) then _shift(_:NUMTYPE_INTEGER, _3) -> _type
        else _shift(_:NUMTYPE_DECIMAL, _3) -> _type
        endif
    else
        _shift(x!KEY!K_NUMBER_TYPE, _3) -> _type
    endif;
    if issimple(y) then
        if isinteger(y) then _:NUMTYPE_INTEGER _add _type -> _type
        else _:NUMTYPE_DECIMAL _add _type -> _type
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
        ERROR       INT_BGINT   AINT_RATIO  INT_DEC     INT_DDEC    CMPLX_ERR   ERROR       ERROR

    ;;;   21          22          23          24          25          26          27          30
    ;;; bgint-int   bgint-bgint bgint-ratio bgint-dec   bgint-ddec  bgint-cmplx   ?           ?
        BGINT_INT   DOBIGINT    AINT_RATIO  BGINT_DEC   BGINT_DDEC  CMPLX_ERR   ERROR       ERROR

    ;;;   31          32          33          34          35          36          37          40
    ;;; ratio-int   ratio-bgint ratio-ratio ratio-dec   ratio-ddec  ratio-cmplx   ?           ?
        RATIO_AINT  RATIO_AINT  DORATIO     RATIO_DEC   RATIO_DDEC  CMPLX_ERR   ERROR       ERROR

    ;;;   41          42          43          44          45          46          47          50
    ;;; dec-int     dec-bgint   dec-ratio   dec-dec     dec-ddec    dec-cmplx     ?           ?
        DEC_INT     DEC_BGINT   DEC_RATIO   DEC_DEC     DEC_DDEC    CMPLX_ERR   ERROR       ERROR

    ;;;   51          52          53          54          55          56          57          60
    ;;; ddec-int    ddec-bgint  ddec-ratio  ddec-dec    ddec-ddec   ddec-cmplx    ?           ?
        DDEC_INT    DDEC_BGINT  DDEC_RATIO  DDEC_DEC    DDEC_DDEC   CMPLX_ERR   ERROR       ERROR

    ;;;   61          62          63          64          65          66          67          70
    ;;; cmplx-int   cmplx-bgint cmplx-ratio cmplx-dec   cmplx-ddec  cmplx-cmplx   ?           ?
        CMPLX_ERR   CMPLX_ERR   CMPLX_ERR   CMPLX_ERR   CMPLX_ERR   CMPLX_ERR   CMPLX_ERR   CMPLX_ERR

    else ERROR;

    INT_BGINT:
        chain(y, BGWEAK Bigint_neg);

    AINT_RATIO:
        chain(_or_=, x, y, RTWEAK Ratio_>, RTWEAK Ratio_op_aint_ratio);

    INT_DEC:
        FLWEAK _pf_dfloat_int(_int(x), FLWEAK _dfop1);
        FLWEAK _pf_dfloat_dec(y, FLWEAK _dfop2);
        goto DODDEC;

    INT_DDEC:
        FLWEAK _pf_dfloat_int(_int(x), FLWEAK _dfop1);
        FLWEAK _pf_dfloat_ddec(y, FLWEAK _dfop2);
        goto DODDEC;

    BGINT_INT:
        return(not(BGWEAK Bigint_neg(x)));

    RATIO_AINT:
        chain(_or_=, x, y, RTWEAK Ratio_>, RTWEAK Ratio_op_ratio_aint);

    BGINT_DEC:
        BGWEAK Bigint_dfloat(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_dec(y, FLWEAK _dfop2);
        goto DODDEC;

    BGINT_DDEC:
        BGWEAK Bigint_dfloat(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_ddec(y, FLWEAK _dfop2);
        goto DODDEC;

    RATIO_DEC:
        RTWEAK Ratio_dfloat(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_dec(y, FLWEAK _dfop2);
        goto DODDEC;

    RATIO_DDEC:
        RTWEAK Ratio_dfloat(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_ddec(y, FLWEAK _dfop2);
        goto DODDEC;

    DEC_INT:
        FLWEAK _pf_dfloat_dec(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_int(_int(y), FLWEAK _dfop2);
        goto DODDEC;

    DEC_BGINT:
        FLWEAK _pf_dfloat_dec(x, FLWEAK _dfop1);
        BGWEAK Bigint_dfloat(y, FLWEAK _dfop2);
        goto DODDEC;

    DEC_RATIO:
        FLWEAK _pf_dfloat_dec(x, FLWEAK _dfop1);
        RTWEAK Ratio_dfloat(y, FLWEAK _dfop2);
        goto DODDEC;

    DEC_DEC:
        FLWEAK _pf_dfloat_dec(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_dec(y, FLWEAK _dfop2);
        goto DODDEC;

    DEC_DDEC:
        FLWEAK _pf_dfloat_dec(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_ddec(y, FLWEAK _dfop2);
        goto DODDEC;

    DDEC_INT:
        FLWEAK _pf_dfloat_ddec(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_int(_int(y), FLWEAK _dfop2);
        goto DODDEC;

    DDEC_BGINT:
        FLWEAK _pf_dfloat_ddec(x, FLWEAK _dfop1);
        BGWEAK Bigint_dfloat(y, FLWEAK _dfop2);
        goto DODDEC;

    DDEC_RATIO:
        FLWEAK _pf_dfloat_ddec(x, FLWEAK _dfop1);
        RTWEAK Ratio_dfloat(y, FLWEAK _dfop2);
        goto DODDEC;

    DDEC_DEC:
        FLWEAK _pf_dfloat_ddec(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_dec(y, FLWEAK _dfop2);
        goto DODDEC;

    DDEC_DDEC:
        FLWEAK _pf_dfloat_ddec(x, FLWEAK _dfop1);
        FLWEAK _pf_dfloat_ddec(y, FLWEAK _dfop2);
        goto DODDEC;


    DOBIGINT:
        chain(_or_=, x, y, BGWEAK Bigint_> );

    DORATIO:
        chain(_or_=, x, y, RTWEAK Ratio_> );

    DODDEC:
        (FLWEAK _dfop1, FLWEAK _dfop2);
        if _or_= then
            _srchain(FLWEAK _pfsgreq)
        else
            _srchain(FLWEAK _pfsgr)
        endif;

    CMPLX_ERR:
        mishap(x, y, 2, 'REAL NUMBER(S) NEEDED');

    ERROR:
        mishap(x, y, 2, 'NUMBER(S) NEEDED')
enddefine;      /* Arith_> */


define 6 x > y;
    lvars x, y;
    if isinteger(x) and isinteger(y) then
        x fi_> y
    else
        Arith_>(x, y, false)
    endif
enddefine;

define 6 x >= y;
    lvars x, y;
    if isinteger(x) and isinteger(y) then
        x fi_>= y
    else
        Arith_>(x, y, true)
    endif
enddefine;

define 6 x < y;
    lvars x, y;
    if isinteger(x) and isinteger(y) then
        x fi_< y
    else
        Arith_>(y, x, false)
    endif
enddefine;

define 6 x <= y;
    lvars x, y;
    if isinteger(x) and isinteger(y) then
        x fi_<= y
    else
        Arith_>(y, x, true)
    endif
enddefine;

define min(x, y);
    lvars x, y;
    if x < y then x else y endif
enddefine;

define max(x, y);
    lvars x, y;
    if x > y then x else y endif
enddefine;


/*  We don't really want this, but it's problematical as an autoloadable
    file because of the name (contains shell metacharacter <, not a valid
    filename in VMS, etc), and we can't really withdraw it altogether.
    Make it vars in case anyone's using it for something else.
*/
vars 6 =<  =  nonop <= ;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 21 1992
        Added =< (reluctantly)
--- John Gibson, Aug 12 1988
        Used chaining of procedures where possible
 */
