/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/src/complex_ops.p
 > Purpose:
 > Author:          John Gibson, Jan 24 1988 (see revisions)
 */


;;; ---------------- OTHER COMPLEX OPERATIONS -------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

section $-Sys;

constant
        procedure (Checkr_num_class, Get_complex, Complex_op_real_cmplx)
    ;

endsection;

;;; -----------------------------------------------------------------------

section $-Sys =>  +:  -:  unary_+:  unary_-:  destcomplex  conjugate;


;;; ---  +: AND -: ---------------------------------------------------------

    ;;; complex x + i*complex y
define lconstant Cmplx_+:(x, y);
    lvars x, y;
    Get_complex(x!CX_REAL - y!CX_IMAG, x!CX_IMAG + y!CX_REAL)
enddefine;

    ;;; complex x - i*complex y
define lconstant Cmplx_-:(x, y);
    lvars x, y;
    Get_complex(x!CX_REAL + y!CX_IMAG, x!CX_IMAG - y!CX_REAL)
enddefine;

define lconstant Cx_|_arith2(x, y, op);
    lvars x, y, op, _type;
    _shift(Checkr_num_class(x) _sub _:NUMTYPE_RATIO, _2) -> _type;
    go_on _pint(Checkr_num_class(y) _sub (_:NUMTYPE_RATIO _sub _1) _add _type) to
        ;;;   00      01      02      03
        ;;; rat-rat rat-dec rat-ddc rat-cpx
            CONSCPX RAT_DEC RAT_DDC RAT_CPX
        ;;;   10      11      12      13
        ;;; dec-rat dec-dec dec-ddc dec-cpx
            DEC_RAT CONSCPX DEC_DDC DEC_CPX
        ;;;   20      21      22      23
        ;;; ddc-rat ddc-dec ddc-ddc ddc-cpx
            DDC_RAT DDC_DEC DDC_DDC DDC_CPX
        ;;;   30      31      32      33
        ;;; cpx-rat cpx-dec cpx-ddc cpx-cpx
            CPX_RAT CPX_DEC CPX_DDC CPX_CPX
    else CPX_CPX;   ;;; can't happen

    RAT_DEC:
        _:ARGTYPE_DECIMAL -> _type;  goto FLOAT_x;

    RAT_DDC:
        _:ARGTYPE_DDECIMAL -> _type;  goto FLOAT_x;

    RAT_CPX:
        0 -> _type;  goto REAL_CPX;

    DEC_RAT:
        _:ARGTYPE_DECIMAL -> _type;  goto FLOAT_y;

    DEC_DDC:
        _:ARGTYPE_DDECIMAL -> _type;
        if FLWEAK popdprecision then goto FLOAT_x else goto FLOAT_y endif;

    DEC_CPX:
        0.0s0 -> _type;  goto REAL_CPX;

    DDC_RAT:
        _:ARGTYPE_DDECIMAL -> _type;  goto FLOAT_y;

    DDC_DEC:
        _:ARGTYPE_DDECIMAL -> _type;
        if FLWEAK popdprecision then goto FLOAT_y else goto FLOAT_x endif;

    DDC_DDC:
        if FLWEAK popdprecision then goto CONSCPX endif;
        _:ARGTYPE_DECIMAL -> _type;  goto FLOAT_x;

    DDC_CPX:
        FLWEAK double_float_0 -> _type;  goto REAL_CPX;

    CPX_RAT:
        0 -> _type;  goto CPX_REAL;

    CPX_DEC:
        0.0s0 -> _type;  goto CPX_REAL;

    CPX_DDC:
        FLWEAK double_float_0 -> _type;  goto CPX_REAL;

    CPX_CPX:
        return(op(x, y));

    REAL_CPX:
        ;;; _type contains 0 of the appropriate type for imag part
        return(Complex_op_real_cmplx(x, _type, y, op));

    CPX_REAL:
        ;;; _type contains 0 of the appropriate type for imag part
        return(Complex_op_cmplx_real(x, y, _type, op));

    FLOAT_x:
        FLWEAK Dfloat(x, FLWEAK _dfop1) -> ;
        FLWEAK Consdecimal(_type, FLWEAK _dfop1) -> x;
        if issimple(x) and iscompound(y) then goto DEC_DDC endif;
        goto CONSCPX;

    FLOAT_y:
        FLWEAK Dfloat(y, FLWEAK _dfop1) -> ;
        FLWEAK Consdecimal(_type, FLWEAK _dfop1) -> y;
        if issimple(y) and iscompound(x) then goto DDC_DEC endif;
        ;;; drop thru to CONSCPX

    CONSCPX:
        Get_complex(x, if op == Cmplx_-: then -y else y endif)
enddefine;

define 5 +: with_nargs 2;
    Cx_|_arith2(Cmplx_+:)
enddefine;

define 5 -: with_nargs 2;
    Cx_|_arith2(Cmplx_-:)
enddefine;

define lconstant Cx_|_arith1(x, _plus);
    lvars x, y, _plus;
    go_on _pint(Checkr_num_class(x) _sub (_:NUMTYPE_RATIO _sub _1)) to
        ;;;   0      1      2      3
             RAT    DEC    DDC    CPX
    else CPX;   ;;; can't happen

    RAT:    0 -> y;  goto CONSCPX;

    DEC:    0.0s0 -> y;  goto CONSCPX;

    DDC:    FLWEAK double_float_0 -> y;  goto CONSCPX;

    CPX:    x!CX_IMAG -> y;  if _plus then -y -> y endif;
            x!CX_REAL -> x;
            ;;; drop thru to CONSCPX

    CONSCPX:
        Get_complex(y, if _plus then x else -x endif)
enddefine;

define unary_+:() with_nargs 1;
    Cx_|_arith1(true)
enddefine;

define unary_-:() with_nargs 1;
    Cx_|_arith1(false)
enddefine;


;;; --- MISCELLANEOUS -----------------------------------------------------

    ;;; complex dest number x
define destcomplex(x);
    lvars x, _ntype;
    if (Checkr_num_class(x) ->> _ntype) == _:NUMTYPE_COMPLEX then
        x!CX_REAL, x!CX_IMAG
    else
        x,  if _ntype == _:NUMTYPE_RATIO then 0
            elseif _ntype == _:NUMTYPE_DECIMAL then
                0.0s0
            else
                FLWEAK double_float_0
            endif
    endif
enddefine;

    ;;; conjugate of number x
define conjugate(x);
    lvars x;
    if Checkr_num_class(x) == _:NUMTYPE_COMPLEX then
        Get_complex(x!CX_REAL, -(x!CX_IMAG))
    else
        x
    endif
enddefine;


endsection;     /* $-Sys */

/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 10 1998
        Replaced single*_float_0 with 0.0s0
--- John Gibson, Nov 30 1993
        Changed CONS label to CONSCPX so it doesn't autoload CONS in VMS
 */
