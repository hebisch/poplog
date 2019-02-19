/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/intgr_arith.p
 > Purpose:
 > Author:          John Gibson, Jan 22 1988 (see revisions)
 */

;;; -------------------- INTEGRAL ARITHMETIC ----------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

global constant
        procedure (Sys$-Int_overflow_op, Sys$-Bigint_rem, Sys$-Bigint_gcd),
        _padd_testovf, _psub_testovf
    ;

;;; ------------------------------------------------------------------------

section $-Sys => gcd_n lcm_n;


;;; --- INTEGRAL ADDITION, SUBTRACTION, ETC ---------------------------------

    ;;; arith op of 2 args, at least one a bigint
define lconstant Intgr_2(x, y, _opsub);
    lvars x, y, _opsub;
    fast_subscrv(_opsub, BGWEAK Bigint_ops) -> _opsub;
    if issimple(y) then
        chain(x, y, _opsub, BGWEAK Bigint_op_bgint_int)
    elseif issimple(x) then
        chain(x, y, _opsub, BGWEAK Bigint_op_int_bgint)
    else
        fast_chain(x, y, _opsub)
    endif
enddefine;

    ;;; add
define Intgr_+(x, y);
    lvars x, y;
    if issimple(x) and issimple(y) then
        if _padd_testovf(x, y) then
            return()
        else
            -> ;
            chain(x, y, OP_+, Int_overflow_op)
        endif
    else
        chain(x, y, OP_+, Intgr_2)
    endif
enddefine;

    ;;; subtract
define Intgr_-(x, y);
    lvars x, y;
    if issimple(x) and issimple(y) then
        if _psub_testovf(x, y) then
            return()
        else
            -> ;
            chain(x, y, OP_-, Int_overflow_op)
        endif
    else
        chain(x, y, OP_-, Intgr_2)
    endif
enddefine;

    ;;; multiply
define Intgr_*(x, y);
    lvars x, y;
    if x == 1 then
        y
    elseif y == 1 then
        x
    elseif issimple(x) and issimple(y) then
        if _pmult_testovf(x, y) then
            return()
        else
            -> ;
            chain(x, y, OP_*, Int_overflow_op)
        endif
    else
        chain(x, y, OP_*, Intgr_2)
    endif
enddefine;

    ;;; quotient and remainder
define Intgr_//(x, y);
    lvars x, y;
    if issimple(x) and issimple(y) then
        x fi_// y
    else
        chain(x, y, OP_//, Intgr_2)
    endif
enddefine;

define Intgr_negate(x);
    lvars x;
    if issimple(x) then
        if _psub_testovf(0, x) then
            return()
        else
            -> ;
            chain(0, x, OP_-, Int_overflow_op)
        endif
    else
        fast_chain(x, fast_subscrv(OP_negate, BGWEAK Bigint_ops))
    endif
enddefine;


;;; --- GREATEST COMMON DIVISOR & LEAST COMMON MULTIPLE ----------------------

    /*  gcd of two non-zero (big)integers
    */
define Intgr_gcd(u, v);
    lvars u, v;

    ;;; gcd of two integers, where _v possibly zero, _u not
    define lconstant Int_gcd(_u, _v);
        lvars t, _u, _v, _k;

            /* find lowest bit in x /= 0 */
        define lconstant Int_odd_shift(x) -> x -> _n;
            lvars x, _n = _0;
            until x _bitst _1 do
                _n _add _1 -> _n;
                _shift(x, _-1) -> x
            enduntil
        enddefine;

        if _zero(_int(_v) ->> _v) then return(_u) endif;
        _int(_u) -> _u;
        if _neg(_u) then
            if _neg(_v) then _negate(_v) else _v endif, _u -> _v -> _u;
        elseunless _neg(_v) then
            _negate(_v) -> _v
        endif;
        if _u == _1 or _v == _-1 then return(1) endif;
        Int_odd_shift(_u) -> _u -> _k;
        Int_odd_shift(_v) -> _v -> t;
        if t _lt _k then t -> _k endif;

        until _zero(_u _add _v ->> t) do
            until t _bitst _1 do _shift(t, _-1) -> t enduntil;
            if _neg(t) then t -> _v else t -> _u endif
        enduntil;
        _pint(_shift(_u, _k))
    enddefine;      /* Int_gcd */

    if issimple(v) then
        if issimple(u) then
            Int_gcd(u, v)
        else
            Int_gcd(v, BGWEAK Bigint_op_bgint_int(u, v, BGWEAK Bigint_rem))
        endif
    elseif issimple(u) then
        Int_gcd(u, BGWEAK Bigint_op_bgint_int(v, u, BGWEAK Bigint_rem))
    else
        BGWEAK Bigint_gcd(u, v) ;;; see bigint_gcd.p
    endif
enddefine;

    /* gcd of _n integers */
define gcd_n(_n);
    lvars _n;
    dlvars i, gd = 0;
    Check_integer(_n, 0);
    _CLAWBACK_SAVE;
    until _n == 0 do
        _n fi_- 1 -> _n;
        nextif((->> i) == 0);       ;;; next integer off stack
        Check_integral(i);
        if gd == 0 then
            i -> gd
        else
            procedure();
                dlocal _nextfree_save;
                Clawback(Intgr_gcd(i, gd)) -> gd
            endprocedure()
        endif
    enduntil;
    Clawback(abs(gd))
enddefine;

    /* lcm of _n integers */
define lcm_n(_n);
    lvars _n;
    dlvars i, lm = false;
    if _n == 0 then return(1) endif;
    Check_integer(_n, 0);
    _CLAWBACK_SAVE;
    until _n == 0 do
        _n fi_- 1 -> _n;
        if (->> i) == 0 then erasenum(_n), return(0) endif;
        Check_integral(i);
        if lm then
            procedure();
                dlocal _nextfree_save;
                Intgr_//(i, Intgr_gcd(lm, i)) -> i -> ;
                Clawback(Intgr_*(lm, i)) -> lm
            endprocedure()
        else
            i -> lm
        endif
    enduntil;
    Clawback(abs(lm))
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 12 1988
        Used chaining of procedures where possible
 */
