/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/complex.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;;--------------------- COMPLEX NUMBERS --------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'gctypes.ph'

global vars
        pop_reduce_ratios
    ;

section $-Sys;

constant
        procedure (Eq__Float, Cxf_arith_2, Cxf_dec_->_ddec,
        Cxf_is_float_real, Complex_abs, Complex_sign
        )
    ;

endsection;

;;; ------------------------------------------------------------------------

section $-Sys => iscomplex complex_key;

;;; --- CALLING COMPLEX OPS WITH 1 OR 2 REAL ARGS --------------------------


lconstant
    work_complex = writeable struct COMPLEX =>> {%0, complex_key, 0%};

define Complex_op_cmplx_real(op) with_nargs 4;
    lvars saver, procedure op, _work = work_complex, _savei;
    _work!CX_REAL -> saver;
    _work!CX_IMAG -> _savei;
    () -> _work!CX_IMAG;        ;;; zero of the appropriate type off stack
    () -> _work!CX_REAL;        ;;; real arg off stack
    op(_work);                  ;;; other arg on stack
    saver -> _work!CX_REAL;
    _savei -> _work!CX_IMAG
enddefine;

define Complex_op_real_cmplx(complex, op) with_nargs 4;
    lvars saver, complex, procedure op, _work = work_complex, _savei;
    _work!CX_REAL -> saver;
    _work!CX_IMAG -> _savei;
    () -> _work!CX_IMAG;        ;;; zero of the appropriate type off stack
    () -> _work!CX_REAL;        ;;; real arg off stack
    op(_work, complex);
    saver -> _work!CX_REAL;
    _savei -> _work!CX_IMAG
enddefine;


;;; --- GENERAL ------------------------------------------------------

    /*  Get a new complex record. Either both real and imag are rational,
        or they are float of the same type, or float of different type.
        The only way the latter case can occur is if a decimal overflows to
        a ddecimal, in which case convert the other to a double also -- so that
        both parts are of the same type.
    */
define Get_complex(real, imag) -> complex;
    lvars complex, real, imag;
    if issimple(imag) then
        if isinteger(imag) then
            if imag == 0 then return(real -> complex) endif
        elseunless issimple(real) then
            FLWEAK Cxf_dec_->_ddec(imag) -> imag
        endif
    elseunless iscompound(real) or isinteger(real) then
        FLWEAK Cxf_dec_->_ddec(real) -> real
    endif;
    Get_store(@@(struct COMPLEX)++) -> complex;
    complex_key -> complex!KEY;
    real -> complex!CX_REAL;
    imag -> complex!CX_IMAG
enddefine;


;;; --- ARITHMETIC ---------------------------------------------------

    ;;; complex x + complex y
define lconstant Cmplx_+(x, y);
    lvars x, y;
    Get_complex(x!CX_REAL + y!CX_REAL, x!CX_IMAG + y!CX_IMAG)
enddefine;

    ;;; complex x - complex y
define lconstant Cmplx_-(x, y);
    lvars x, y;
    Get_complex(x!CX_REAL - y!CX_REAL, x!CX_IMAG - y!CX_IMAG)
enddefine;

    ;;; complex x * complex y
define lconstant Cmplx_*(x, y);
    lvars x, y;
    if testdef decimal_key
    and (FLWEAK isdecimal(x!CX_REAL) or FLWEAK isdecimal(y!CX_REAL)) then
        ;;; do floating-point version
        FLWEAK Cxf_arith_2(x, y, FLWEAK Cxf_mult)
    else
        _CLAWBACK_SAVE;
        dlocal RTWEAK pop_reduce_ratios = false;
        x!CX_REAL*y!CX_REAL, x!CX_IMAG*y!CX_IMAG;   ;;; ac, bd
        true -> RTWEAK pop_reduce_ratios;
        Clawback_num(() - ());                      ;;; ac - bd

        false -> RTWEAK pop_reduce_ratios;
        x!CX_IMAG*y!CX_REAL, x!CX_REAL*y!CX_IMAG;   ;;; bc, ad
        true -> RTWEAK pop_reduce_ratios;
        Clawback_num(() + ());                      ;;; bc + ad

        Get_complex((), ())
    endif
enddefine;

    ;;; complex x / complex y
define lconstant Cmplx_/(x, y);
    lvars x, y, d;
    if testdef decimal_key
    and (FLWEAK isdecimal(x!CX_REAL) or FLWEAK isdecimal(y!CX_REAL)) then
        ;;; do floating-point version
        FLWEAK Cxf_arith_2(x, y, FLWEAK Cxf_div)
    else
        _CLAWBACK_SAVE;
        dlocal RTWEAK pop_reduce_ratios = false;
        y!CX_REAL*y!CX_REAL, y!CX_IMAG*y!CX_IMAG;   ;;; c**2, d**2
        true -> RTWEAK pop_reduce_ratios;
        ;;; have to keep this (unfortunately!), because it or parts of it may
        ;;; form part of imag, and could be overwritten by clawing back real
        Clawback_num(() + ()) -> d;         ;;; c**2 + d**2 = abs(y)**2

        false -> RTWEAK pop_reduce_ratios;
        x!CX_REAL*y!CX_REAL + x!CX_IMAG*y!CX_IMAG;  ;;; ac + bd
        true -> RTWEAK pop_reduce_ratios;
        Clawback_num(() / d);                       ;;; real part

        false -> RTWEAK pop_reduce_ratios;
        x!CX_IMAG*y!CX_REAL - x!CX_REAL*y!CX_IMAG;  ;;; bc - ad
        true -> RTWEAK pop_reduce_ratios;
        Clawback_num(() / d);                       ;;; imag part

        Get_complex((), ())
    endif
enddefine;

    ;;; complex x // complex y
define lconstant Cmplx_//(x, y);
    lconstant wc = writeable struct COMPLEX =>> {%0, complex_key, 0%};
    lvars i, y, x, _savedp = FLWEAK popdprecision;
    dlocal FLWEAK popdprecision = true;
    _CLAWBACK_SAVE;
    Clawback_num(intof(Cmplx_/(x, y))) -> i;
    "ddecimal" -> FLWEAK popdprecision;
    if y == work_complex then
        y!CX_REAL -> wc!CX_REAL, y!CX_IMAG -> wc!CX_IMAG;
        wc * i -> y
    else
        y * i -> y;
        if x == work_complex then
            x!CX_REAL -> wc!CX_REAL, x!CX_IMAG -> wc!CX_IMAG;
            wc -> x
        endif
    endif;
    _savedp -> FLWEAK popdprecision;
    Clawback_num(x - y), i
enddefine;


;;; --- COMPLEX ARITH OPERATIONS VECTOR ----------------------------------

define lconstant Cmplx_1arg(x, pdr);
    lvars x, procedure pdr;
    Get_complex(pdr(x!CX_REAL), pdr(x!CX_IMAG))
enddefine;

define lconstant Cmplx_1arg_float() with_nargs 2;
    if testdef decimal_key then
        fast_apply()
    else
        -> ;        ;;; erase op procedure
        mishap((), 1, 'INVALID OPERATION ON COMPLEX NUMBER (floating-point not loaded)')
    endif
enddefine;

constant
    Complex_ops = initv(OP_VEC_LEN);

    Cmplx_+                 -> subscrv(OP_+, Complex_ops),
    Cmplx_-                 -> subscrv(OP_-, Complex_ops),
    Cmplx_*                 -> subscrv(OP_*, Complex_ops),
    Cmplx_/                 -> subscrv(OP_/, Complex_ops),
    Cmplx_//                -> subscrv(OP_//, Complex_ops),
    Cmplx_1arg(%negate%)    -> subscrv(OP_negate, Complex_ops),
    Cmplx_1arg_float(%FLWEAK Complex_abs%)
                            -> subscrv(OP_abs, Complex_ops),
    Cmplx_1arg(%fracof%)    -> subscrv(OP_fracof, Complex_ops),
    Cmplx_1arg(%intof%)     -> subscrv(OP_intof, Complex_ops),
    Cmplx_1arg(%round%)     -> subscrv(OP_round, Complex_ops),
    Cmplx_1arg_float(%FLWEAK Complex_sign%)
                            -> subscrv(OP_sign, Complex_ops),
    ;


;;; ----------------------------------------------------------------------

define Complex_is_float_real(/* complex, _df_opnd */) with_nargs 2;
    if testdef decimal_key then
        FLWEAK Cxf_is_float_real()
    else
        -> -> ;     ;;; erase args
        false
    endif
enddefine;


;;; --- COMPLEX KEY -----------------------------------------------------

define iscomplex(item);
    lvars item;
    iscompound(item) and item!KEY == complex_key
enddefine;

define lconstant Eq__Complex(item, complex);
    lvars item, complex, _key;
    if iscompound(item) then
        if (item!KEY ->> _key) == complex_key then
            return( EQ(item!CX_REAL, complex!CX_REAL)
                    and EQ(item!CX_IMAG, complex!CX_IMAG) )
        elseif _key!K_FLAGS _bitst _:M_K_MATCH_VAR then
            fast_chain(complex, item, _key!K_SYS_EQUALS)
        endif
    endif;
    ;;; else can only be equal if complex is a float-complex
    ;;; with 0.0 imaginary part
    Complex_is_float_real(complex, FLWEAK _dfop1)
        and FLWEAK Eq__Float(item, complex!CX_REAL)
enddefine;

define lconstant Cmplx_print(x);
    lvars x, imag;
    pr(x!CX_REAL);
    x!CX_IMAG -> imag;
    if imag < 0 then
        -imag -> imag, '_-:'
    else
        '_+:'
    endif;
    printf(), pr(imag)
enddefine;

define lconstant Cmplx_hash(item);
    lvars item;
    syshash(item!CX_REAL) fi_+ syshash(item!CX_IMAG)
enddefine;


constant
    complex_key = struct KEY_R =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL_RECORD _biset _:M_K_NONWRITEABLE,
                                ;;; K_FLAGS
        _:GCTYPE_FULLREC2,      ;;; K_GC_TYPE
        Rec2_getsize,           ;;; K_GET_SIZE

        "complex",              ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        iscomplex,              ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        Eq__Complex,            ;;; K_SYS_EQUALS
        WREF Eq__Complex,       ;;; K_EQUALS
        Cmplx_print,            ;;; K_SYS_PRINT
        WREF Cmplx_print,       ;;; K_PRINT
        WREF Cmplx_hash,        ;;; K_HASH

        _:NUMTYPE_COMPLEX,      ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct COMPLEX)++,   ;;; K_RECSIZE_R
        false,                  ;;; K_CONS_R
        false,                  ;;; K_DEST_R
        false,                  ;;; K_ACCESS_R
        %};


define Complex_clawback(_cx);
    lvars real, imag, _cx;
    _cx!CX_REAL -> real;
    _cx!CX_IMAG -> imag;
    if iscompound(real) and iscompound(imag) and imag <@(w) real then
        Clawback_num(imag) -> _cx!CX_IMAG, Clawback_num(real) -> _cx!CX_REAL
    else
        Clawback_num(real) -> _cx!CX_REAL, Clawback_num(imag) -> _cx!CX_IMAG
    endif;
    Clawback(_cx)
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan  4 1996
        Changed to use EQ macro
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Dec  4 1989
        Changes for new pop pointers
--- John Gibson, Feb  9 1988
        Changes for splitting up files, sectioning, putting in weakrefs, etc.
 */
