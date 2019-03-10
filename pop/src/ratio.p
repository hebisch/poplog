/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/ratio.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;;-------------------------- RATIOS ------------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'gctypes.ph'

section $-Sys;

constant
        procedure (Intgr_+, Intgr_-, Intgr_*, Intgr_//, Intgr_negate,
        Intgr_gcd, Bigint_>, Eq__Bigint, Bigint_->_dfloat, Bigint_/_dfloat,
        Bigint_copy
        )
    ;

endsection;

;;; ------------------------------------------------------------------------

section $-Sys => pop_pr_ratios, pop_reduce_ratios,
                 destratio, isratio, isrational, ratio_key;

vars
    pop_pr_ratios       = true,
    pop_reduce_ratios   = true,
    ;


;;; --- CALLING RATIO OPS WITH 1 OR 2 BIG/INTEGER ARGS ----------------------

lconstant
    work_ratio = writeable struct RATIO =>> {%0, ratio_key, 1%};

define Ratio_op_ratio_aint(op) with_nargs 3;
    lvars save, procedure op, _work = work_ratio;
    _work!RAT_NUMER -> save;
    () -> _work!RAT_NUMER;  ;;; int arg off stack
    op(_work);              ;;; other on stack
    save -> _work!RAT_NUMER
enddefine;

define Ratio_op_aint_ratio(ratio, op) with_nargs 3;
    lvars save, ratio, procedure op, _work = work_ratio;
    _work!RAT_NUMER -> save;
    () -> _work!RAT_NUMER;  ;;; int arg off stack
    op(_work, ratio);
    save -> _work!RAT_NUMER
enddefine;


;;; --- INTEGRAL ROUTINES -------------------------------------------------

define lconstant Intgr_neg(x);
    lvars x;
    if issimple(x) then
        x fi_< 0
    else
        chain(x, BGWEAK Bigint_neg)
    endif
enddefine;

    ;;; sign(x - y)
define lconstant Intgr_-_sign(x, y);
    lvars x, y;
    if issimple(x) then
        if issimple(y) then
            if x == y then 0 elseif x fi_> y then 1 else -1 endif
        else
            if BGWEAK Bigint_neg(y) then 1 else -1 endif
        endif
    elseif issimple(y) then
        if BGWEAK Bigint_neg(x) then -1 else 1 endif
    else
        BGWEAK Bigint_>(0, x, y) -> x;      ;;; returns 0 if =
        if x == 0 then 0 elseif x then 1 else -1 endif
    endif
enddefine;


;;; --- GENERAL ------------------------------------------------------

    ;;; get a ratio
define lconstant Get_ratio(num, den) -> ratio;
    lvars ratio, num, den;
    if den == 1 then
        ;;; return integral result
        num -> ratio
    else
        ;;; return ratio
        Get_store(@@(struct RATIO)++) -> ratio;
        ratio_key -> ratio!KEY;
        num -> ratio!RAT_NUMER;
        den -> ratio!RAT_DENOM;
    endif
enddefine;

    /*  reduce integers u and v to their lowest common terms, with v +ve
    */
define lconstant Rat_reduce(u, v);
    lvars g, u, v;
    if u == 0 then
        return(0, 1)
    elseif pop_reduce_ratios then
        Intgr_gcd(u, v) -> g;       ;;; v /== 0 (see intarith.p)
        ;;; g is now the gcd -- make sure sign(g) = sign(v) so that the
        ;;; denominator v/g is positive
        if Intgr_neg(v) /== Intgr_neg(g) then
            Intgr_negate(g) -> g
        endif;
        ;;; then divide u, v by the gcd
        unless g == 1 then
            Intgr_//(u, g) -> (, u);
            Intgr_//(v, g) -> (, v)
        endunless
    endif;
    u, v
enddefine;

    ;;; construct a ratio result from (big)integers x, y
define lconstant Rat_cons() with_nargs 2;
    lvars x, y;
    Rat_reduce(/* x, y*/) -> y -> x;
    if iscompound(x) and iscompound(y) and y <@(w) x then
        Clawback(y) -> y, Clawback(x), y
    else
        Clawback(x), Clawback(y)
    endif;
    Get_ratio()
enddefine;


;;; --- ARITHMETIC ---------------------------------------------------

    ;;; construct a ratio x/y when dividing (big)integers
    ;;; (musn't be called with either argument a working bigint)
define Ratio_cons() with_nargs 2;
    _CLAWBACK_SAVE;
    Rat_cons(/* x, y */)        ;;; return the ratio
enddefine;

    ;;; ratio x + ratio y
define lconstant Rat_+(x, y);
    lvars x, y, f, g;
    _CLAWBACK_SAVE;
    Rat_cons( Intgr_+( Intgr_*(x!RAT_NUMER, y!RAT_DENOM),
                        Intgr_*(y!RAT_NUMER, x!RAT_DENOM) ),
                Intgr_*(y!RAT_DENOM, x!RAT_DENOM) )
enddefine;

    ;;; ratio x - ratio y
define lconstant Rat_-(x, y);
    lvars x, y, f, g;
    _CLAWBACK_SAVE;
    Rat_cons( Intgr_-( Intgr_*(x!RAT_NUMER, y!RAT_DENOM),
                        Intgr_*(y!RAT_NUMER, x!RAT_DENOM) ),
                Intgr_*(y!RAT_DENOM, x!RAT_DENOM) )
enddefine;

    ;;; ratio x * ratio y
define lconstant Rat_*(x, y);
    lvars x, y;
    _CLAWBACK_SAVE;
    Rat_cons( Intgr_*(x!RAT_NUMER, y!RAT_NUMER),
                    Intgr_*(x!RAT_DENOM, y!RAT_DENOM) )
enddefine;

    ;;; ratio x / ratio y
define lconstant Rat_/(x, y);
    lvars x, y;
    _CLAWBACK_SAVE;
    Rat_cons( Intgr_*(x!RAT_NUMER, y!RAT_DENOM),
                    Intgr_*(x!RAT_DENOM, y!RAT_NUMER) )
enddefine;

    ;;; ratio x // ratio y
define lconstant Rat_//(x, y);
    lvars x, y, i, xd, yd;
    _CLAWBACK_SAVE;
    x!RAT_DENOM -> xd;
    y!RAT_DENOM -> yd;
    Intgr_//( Intgr_*(x!RAT_NUMER, yd), Intgr_*(xd, y!RAT_NUMER) )
                                        -> i -> x;
    Rat_reduce(x, Intgr_*(xd, yd)) -> y -> x;
    if iscompound(i) then BGWEAK Bigint_copy(i) -> i endif;
    if iscompound(x) and iscompound(y) and y <@(w) x then
        Clawback(y) -> y, Clawback(x), y
    else
        Clawback(x), Clawback(y)
    endif;
    Clawback(i) -> i;
    Get_ratio(), i
enddefine;

    ;;; negation of ratio x
define lconstant Rat_negate(x);
    lvars x;
    Get_ratio( Intgr_negate(x!RAT_NUMER), x!RAT_DENOM )
enddefine;

    ;;; absolute value of ratio x
define lconstant Rat_abs(x);
    lvars x;
    if Intgr_neg(x!RAT_NUMER) then
        Rat_negate(x)
    else
        x
    endif
enddefine;

    ;;; integer part of ratio x
define lconstant Rat_intof(x);
    lvars x;
    _CLAWBACK_SAVE;
    Intgr_//(x!RAT_NUMER, x!RAT_DENOM) -> x ->;
    Clawback(x)
enddefine;

    ;;; fractional part of ratio x
define lconstant Rat_fracof(x);
    lvars x;
    _CLAWBACK_SAVE;
    Get_ratio( Clawback(Intgr_//(x!RAT_NUMER, x!RAT_DENOM) ->), x!RAT_DENOM)
enddefine;

define lconstant Rat_round(x);
    lvars x, rm, q, i;
    _CLAWBACK_SAVE;
    Intgr_//(x!RAT_NUMER, x!RAT_DENOM) -> q -> rm;
    unless rm == 0 then
        if Intgr_neg(rm) then
            Intgr_*(rm, -2) -> rm;  -1 -> i
        else
            rm << 1 -> rm;  1 -> i
        endif
    endunless;
    if Intgr_-_sign(rm, x!RAT_DENOM) fi_>= 0 then
        ;;; round it
        Intgr_+(q, i) -> q
    endif;
    Clawback(q)
enddefine;

define lconstant Rat_sign(x);
    lvars x;
    if Intgr_neg(x!RAT_NUMER) then -1 else 1 endif
enddefine;

    ;;; convert ratio x to a double float
define Ratio_dfloat(x, _df_opnd);
    lvars x, y, _df_opnd;
    lstackmem dfloat _df_work;
    lconstant macro BGFLWEAK = [weakref[Bigint_->_dfloat]];

    x!RAT_DENOM -> y;
    x!RAT_NUMER -> x;
    if issimple(x) and issimple(y) then
        FLWEAK _pf_dfloat_int(_int(x), _df_opnd);
        FLWEAK _pf_dfloat_int(_int(y), _df_work);
        FLWEAK _pfdiv(_df_opnd, _df_work) ->
    elseif issimple(y) then
        chain(_df_opnd, x, y, BGFLWEAK Bigint_/_dfloat,
                                            BGFLWEAK Bigint_op_bgint_int)
    elseif issimple(x) then
        chain(_df_opnd, x, y, BGFLWEAK Bigint_/_dfloat,
                                            BGFLWEAK Bigint_op_int_bgint)
    else
        chain(_df_opnd, x, y, BGFLWEAK Bigint_/_dfloat)
    endif
enddefine;

    ;;; numerator and denominator of a rational
define destratio(x);
    lvars x, _key;
    if issimple(x) then
        if isinteger(x) then return(x, 1) endif
    elseif (x!KEY ->> _key) == ratio_key then
        return(x!RAT_NUMER, x!RAT_DENOM)
    elseif _key == weakref biginteger_key then
        return(x, 1)
    endif;
    mishap(x, 1, 'RATIONAL NUMBER NEEDED')
enddefine;


;;; --- RATIO ARITH OPERATION VECTOR ------------------------------------

constant
    Ratio_ops = initv(OP_VEC_LEN);

    Rat_+       -> subscrv(OP_+, Ratio_ops),
    Rat_-       -> subscrv(OP_-, Ratio_ops),
    Rat_*       -> subscrv(OP_*, Ratio_ops),
    Rat_/       -> subscrv(OP_/, Ratio_ops),
    Rat_//      -> subscrv(OP_//, Ratio_ops),
    Rat_negate  -> subscrv(OP_negate, Ratio_ops),
    Rat_abs     -> subscrv(OP_abs, Ratio_ops),
    Rat_fracof  -> subscrv(OP_fracof, Ratio_ops),
    Rat_intof   -> subscrv(OP_intof, Ratio_ops),
    Rat_round   -> subscrv(OP_round, Ratio_ops),
    Rat_sign    -> subscrv(OP_sign, Ratio_ops),
    ;


;;; --- COMPARISON -------------------------------------------------------

define Ratio_>(_or_=, x, y);
    lvars x, y, _tcmp, _dcmp, _or_=;
    if Intgr_neg(x!RAT_NUMER) then
        if Intgr_neg(y!RAT_NUMER) then
            ;;; both negative
            Intgr_-_sign(x!RAT_DENOM, y!RAT_DENOM) -> _dcmp
        else
            ;;; x negative, y positive
            return(false)
        endif
    elseif Intgr_neg(y!RAT_NUMER) then
        ;;; x positive, y negative
        return(true)
    else
        ;;; both positive
        Intgr_-_sign(y!RAT_DENOM, x!RAT_DENOM) -> _dcmp
    endif;
    Intgr_-_sign(x!RAT_NUMER, y!RAT_NUMER) fi_+ _dcmp -> _tcmp;
    if _tcmp /== 0 then
        return(_tcmp fi_> 0)
    elseif _dcmp == 0 then
        ;;; numerator and denominator equal
        return(_or_=)
    endif;
    ;;; else have to cross multiply ...
    _CLAWBACK_SAVE;
    Intgr_-_sign( Intgr_*(x!RAT_NUMER, y!RAT_DENOM),
                    Intgr_*(x!RAT_DENOM, y!RAT_NUMER) ) fi_> 0;
    Clawback()
enddefine;

define Eq__Ratio(item, ratio);
    lvars item, ratio;

    define lconstant Intgr_=(x, y);
        lvars x, y;
        if issimple(x) then
            x == y
        elseif issimple(y) then
            false
        else
            ;;; both bigints
            chain(x, y, BGWEAK Eq__Bigint)
        endif
    enddefine;

    if isinteger(item) then
        return(false)
    elseif issimple(item) then
        FLWEAK _pf_dfloat_dec(item, FLWEAK _dfop1)
    else
        ;;; structure
        go_on _pint(item!KEY!K_NUMBER_TYPE) to
        ;;; 1    2    3    4     5      6
            NO  NO  RATIO  NO   DDEC  COMPLEX
        else NO;

        RATIO:
            return(Intgr_=(item!RAT_NUMER, ratio!RAT_NUMER)
                    and Intgr_=(item!RAT_DENOM, ratio!RAT_DENOM));

        DDEC:
            FLWEAK _pf_dfloat_ddec(item, FLWEAK _dfop1); goto CMPFLOAT;

        COMPLEX:
            if CXWEAK Complex_is_float_real(item, FLWEAK _dfop1) then
                goto CMPFLOAT
            endif;          ;;; else drop thru to NO

        NO:
            if item!KEY!K_FLAGS _bitst _:M_K_MATCH_VAR then
                fast_chain(ratio, item, item!KEY!K_SYS_EQUALS)
            endif;
            return(false)
    endif;

    CMPFLOAT:
    ;;; drop thru for float comparison
    Ratio_dfloat(ratio, FLWEAK _dfop2);
    FLWEAK _pfeq(FLWEAK _dfop1, FLWEAK _dfop2)
enddefine;


;;; --- PRINTING --------------------------------------------------------

define lconstant Rat_print(x);
    lvars x;
    if pop_pr_ratios or not(testdef decimal_key) then
        sys_syspr(x!RAT_NUMER),
        cucharout(`_`), cucharout(`/`),
        sys_syspr(x!RAT_DENOM)
    else
        ;;; print as floating-point
        Ratio_dfloat(x, FLWEAK _dfop1);
        sys_syspr(FLWEAK Consdecimal(_:ARGTYPE_DDECIMAL, FLWEAK _dfop1))
    endif
enddefine;


;;; --- UNICODE CHARS -> FRACTIONS -------------------------------------

constant unichar_fractions = [
    16:00BC 1_/4  16:00BD 1_/2  16:00BE 3_/4
    16:2153 1_/3  16:2154 2_/3
    16:2155 1_/5  16:2156 2_/5  16:2157 3_/5  16:2158 4_/5
    16:2159 1_/6  16:215A 5_/6
    16:215B 1_/8  16:215C 3_/8  16:215D 5_/8  16:215E 7_/8
];

;;; --- RATIO KEY ------------------------------------------------------

define isratio(item);
    lvars item;
    iscompound(item) and item!KEY == ratio_key
enddefine;

define isrational(item);
    lvars item;
    if iscompound(item) then
        item!KEY == weakref biginteger_key or item!KEY == ratio_key
    else
        isinteger(item)
    endif
enddefine;

define lconstant Rat_hash() with_nargs 1;
    syshash(intof())
enddefine;

constant
    ratio_key = struct KEY_R =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL_RECORD _biset _:M_K_NONWRITEABLE,
                                ;;; K_FLAGS
        _:GCTYPE_FULLREC2,      ;;; K_GC_TYPE
        Rec2_getsize,           ;;; K_GET_SIZE

        "ratio",                ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        isratio,                ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        Eq__Ratio,              ;;; K_SYS_EQUALS
        WREF Eq__Ratio,         ;;; K_EQUALS
        Rat_print,              ;;; K_SYS_PRINT
        WREF Rat_print,         ;;; K_PRINT
        WREF Rat_hash,          ;;; K_HASH

        _:NUMTYPE_RATIO,        ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct RATIO)++,     ;;; K_RECSIZE_R
        false,                  ;;; K_CONS_R
        false,                  ;;; K_DEST_R
        false,                  ;;; K_ACCESS_R
        %};


define Ratio_clawback(_r);
    lvars num, den, _r;
    _r!RAT_NUMER -> num;
    _r!RAT_DENOM -> den;
    if iscompound(num) and iscompound(den) and den <@(w) num then
        Clawback(den) -> _r!RAT_DENOM, Clawback(num) -> _r!RAT_NUMER
    else
        Clawback(num) -> _r!RAT_NUMER, Clawback(den) -> _r!RAT_DENOM
    endif;
    Clawback(_r)
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 24 1997
        Added unichar_fractions
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Dec  4 1989
        Chanegs for new pop pointers
--- John Gibson, Aug 12 1988
        Used chaining of procedures where possible
--- John Gibson, Feb  9 1988
        Changes for splitting up files, sectioning, putting in weakrefs, etc.
--- John Gibson, Dec 18 1987
        Added some missing declarations at top of file
--- John Gibson, Sep  5 1987
        Changed ratio_key to new format
--- John Gibson, Aug 14 1987
        Changed for segmented system
 */
