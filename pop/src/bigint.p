/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/bigint.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;----------------------- BIG INTEGERS --------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'gctypes.ph'

global constant
      _bgi_add, _bgi_sub, _bgi_negate, _bgi_negate_no_ov,
      _bgi_mult, _bgi_mult_add, _bgi_div, _scmp
    ;

section $-Sys;

constant
        procedure (Intgr_//, Bigint_//, Bigint_->_dfloat, Print_int,
        )
    ;

endsection;


;;; -------------------------------------------------------------------------

section $-Sys => isbiginteger, biginteger_key;


constant

    ;;; working bigints used for converting integers
    work_bigint1 = writeable
                    struct BIGINT =>> {%_1, biginteger_key, =>> {%_0%}%},
    work_bigint2 = writeable
                    struct BIGINT =>> {%_1, biginteger_key, =>> {%_0%}%},
    bigint_minus_1 = struct BIGINT =>> {%_1, biginteger_key, =>> {%_-1%}%},
    ;


        ;;; see also numbers.ph

#_IF "BIGINT_SPEC".valof == "short"

    lconstant macro (
        BGI_VEC_TYPE    = t_SHORT,
        BGI_VEC_GETSIZE = "Shortvec_getsize",
        _CMP            = "_scmp",
        );

#_ELSEIF "BIGINT_SPEC".valof == "int"

    lconstant macro (
        BGI_VEC_TYPE    = t_INT,
        BGI_VEC_GETSIZE = "Intvec_getsize",
        _CMP            = "_icmp",
        );

#_ELSE

    mishap("BIGINT_SPEC".valof, 1, 'INVALID BIGINT_SPEC');

#_ENDIF


lconstant macro (

    ;;; max number of bits for a printing slice
    PRINT_SLICE_BITS    = min(POPINT_BITS, SLICE_BITS),

    );


;;; --- GENERAL -----------------------------------------------------------

    ;;; store a pop integer in a working bigint
define Pint_to_bigint(_i, work_bigint) -> work_bigint;
    lvars work_bigint _i;
    _int(_i) -> _i;
#_IF POPINT_SLICES == 2
    lvars _j;
    _shift(_i, _:-SLICE_BITS) -> _j;
    _i _bimask _:SLICE_MASK -> _i;
    lvars _ii = _shift(_i, _:SLICE_BITS);
    if _zero(_j) and not(_neg(_ii))
       or _j == _-1 and _neg(_ii) then
        _1 -> work_bigint!BGI_LENGTH
    else
        ;;; need two slices
        _j -> work_bigint!BGI_SLICES[_1];
        _2 -> work_bigint!BGI_LENGTH;
    endif;
#_ENDIF
    _i -> work_bigint!BGI_SLICES[_0]
enddefine;


define Bigint_neg(bint);
    lvars bint;
    _neg( bint!BGI_SLICES[bint!BGI_LENGTH _sub _1] )
enddefine;


    /*  Return a bigint result, first truncating unwanted sign-extension
        slices, then returning an integer if possible.
        Also returns the new end address of the structure.
        (N.B. bint can be a constant nonwriteable bigint, so musn't be
        changed unless it is truncatable (which a constant won't be).)
    */
define Bigint_return(bint);
    lvars bint, _addr, _x, _lim, _len;

    ;;; truncate unwanted sign extensions
    bint!BGI_LENGTH -> _len;
    bint@BGI_SLICES -> _lim;
    _lim@(SL)[_len] -> _addr;
    if _neg(_addr--!(-SL) -> _addr ->> _x) then
        ;;; negative
        while _x == _-1 and _addr >@(SL) _lim do
            _len _sub _1 -> _len;
            _addr--!(-SL) -> _addr -> _x;
        endwhile;
        if not(_neg(_x)) then
            _len _add _1 -> _len;
        endif;
    else
        ;;; positive
        while _zero(_x) and _addr >@(SL) _lim do
            _len _sub _1 -> _len;
            _addr--!(-SL) -> _addr -> _x
        endwhile;
        if _neg(_x) then
            _len _add _1 -> _len;
        endif;
    endif;

    ;;; _len is now the correct length -- see if can return an integer
    if _len _lteq _:POPINT_SLICES then
#_IF POPINT_SLICES == 2
        if _len == _1 then
            ;;; single must fit
            return(_pint(_x), bint)
        endif;
        _shift(_x, _:SLICE_BITS) _biset (_addr--!(SL) -> _addr) -> _x;
#_ENDIF
        ;;; see whether the resulting int fits into a popint
        if _pint_testovf(_x) then
            return(bint)            ;;; integer result is on stack
        endif
    endif;
    ;;; else return the truncated biginteger and its new end address
    if bint!BGI_LENGTH /== _len then _len -> bint!BGI_LENGTH endif;
    bint, bint@V_WORDS[_len|SL.r]@~POPBASE
enddefine;

    ;;; Get a new biginteger
define Get_bigint(_len) -> bint;
    lvars bint, _len;
    Get_store(@@V_WORDS[_len|SL.r] _sub @@POPBASE) -> bint;
    biginteger_key -> bint!KEY;
    _len -> bint!BGI_LENGTH;
enddefine;

    ;;; copy a bigint to length _len, sign extending if necessary
define Bigint_copy_len(bint, _len) -> result;
    lvars bint, result, _len, _work, _Raddr, _Rlast;
    @@V_WORDS[_len|SL.r] _sub @@POPBASE -> _work;
    Get_store(_work) -> result;
    _moveq(_work, bint@POPBASE, result@POPBASE) -> ;
    _len -> result!BGI_LENGTH;
    if _len _gr bint!BGI_LENGTH then
        ;;; sign extend
        result@BGI_SLICES[bint!BGI_LENGTH] -> _Raddr;
        result@BGI_SLICES[_len _sub _1] -> _Rlast;
        if _neg(_Raddr!(-SL)[_-1] ->> _work) then
            _-1
        else
            _0
        endif -> _work;
        while _Raddr <@(SL) _Rlast do
            _work -> _Raddr!(SL)++ -> _Raddr
        endwhile;
        _work -> _Raddr!(SL)
    endif
enddefine;

    ;;; copy a bigint
define Bigint_copy(bint);
    lvars bint;
    Bigint_copy_len(bint, bint!BGI_LENGTH)
enddefine;


;;; --- CALLING BIGINT OPS WITH 1 OR 2 INTEGER ARGS --------------------------

define :inline lconstant SETWORK(arg, work, call_code);
    SAVEWORKBGI(work, _save1, _save2);
#_IF POPINT_SLICES == 1 \n
    _int(arg) -> work!BGI_SLICES, work;
#_ELSE
    Pint_to_bigint(arg, work);
#_ENDIF
    call_code;
    RESTWORKBGI(work, _save1, _save2)
enddefine;


define Bigint_op_bgint_int(/*_bint, _i,*/ op);
    lvars procedure op, _work1 = work_bigint1;
    SETWORK((), _work1, op())       ;;; other on stack
enddefine;

define Bigint_op_int_bgint(/*_i,*/ _bint, op);
    lvars procedure op, _work1 = work_bigint1, _bint;
    SETWORK((), _work1, op(_bint))
enddefine;

define Bigint_op_int_int(/*_i1,_i2,*/, op);
    lvars procedure op, _work2 = work_bigint2;
    SETWORK((), _work2, Bigint_op_int_bgint(op))
enddefine;


;;; --- ADDITION, SUBTRACTION -------------------------------------------------

    ;;; sign extension of a bigint, given the address of
    ;;; after its last component
define lconstant Bgi_sign_extn() with_nargs 1;
    if _neg(!(-SL)[_-1]) then _-1 else _0 endif;
enddefine;

    ;;; bigint x + bigint y
define lconstant Bgi_+(x, y);
    lvars x, y, result,
          _Rlen, _Xlen, _Ylen
        ;
    x!BGI_LENGTH ->> _Rlen -> _Xlen;
    y!BGI_LENGTH -> _Ylen;
    ;;; get greater of the two lengths in _Rlen
    if _Ylen _gr _Xlen then _Ylen -> _Rlen endif;
    Get_bigint(_Rlen _add _1) -> result;   ;;; 1 extra for overflow

    _bgi_add(x@BGI_SLICES, _Xlen, y@BGI_SLICES, _Ylen,
             result@BGI_SLICES);

    Bigint_return(result) -> Get_store()
enddefine;

    ;;; bigint x - bigint y
define lconstant Bgi_-(x, y);
    lvars x, y, result,
          _Rlen, _Xlen, _Ylen
        ;
    x!BGI_LENGTH ->> _Rlen -> _Xlen;
    y!BGI_LENGTH -> _Ylen;
    ;;; get greater of the two lengths in _Rlen
    if _Ylen _gr _Xlen then _Ylen -> _Rlen endif;
    Get_bigint(_Rlen _add _1) -> result;   ;;; 1 extra for overflow

    _bgi_sub(y@BGI_SLICES, _Ylen, x@BGI_SLICES, _Xlen,
             result@BGI_SLICES);

    Bigint_return(result) -> Get_store()
enddefine;

define Bigint_negate_range(_saddr, _slen, _daddr);
    lvars _saddr, _daddr, _slen;
    _bgi_negate_no_ov(_saddr, _slen, _daddr);
enddefine;

;;; Produce a new negated bigint, this one always produces
;;; bigints, even if result will fit pop int.
define Bigint_do_negate(bint) -> result;
    lvars bint, result, _len;
    bint!BGI_LENGTH -> _len;
    if _neg(bint!BGI_SLICES[_len _sub _1]) then
        Get_bigint(_len _add _1) -> result;     ;;; allow 1 for overflow
        _bgi_negate(bint@BGI_SLICES, _len, result@BGI_SLICES);
        if result!BGI_SLICES[_len _sub _1] _sgreq _0 then
            ;;; no overflow
            _len -> result!BGI_LENGTH;
            result@V_WORDS[_len|SL.r]@~POPBASE -> Get_store()
        endif;
    else
        Get_bigint(_len) -> result;
        Bigint_negate_range(bint@BGI_SLICES, _len, result@BGI_SLICES);
    endif;
enddefine;

define lconstant Bgi_negate() with_nargs 1;
    lvars result = Bigint_do_negate();
    Bigint_return(result) -> Get_store();
enddefine;

define lconstant Bgi_abs(bint);
    lvars bint;
    if Bigint_neg(bint) then
        Bgi_negate(bint);
    else
        bint
    endif
enddefine;


;;; --- MULTIPLICATION -------------------------------------------------------

define lconstant Bgi_*(x, y);
    lvars y, x, result, _Yaddr, _Raddr, _Xlen, _Ylen, _Xstart,
          need_neg = false;

    x!BGI_LENGTH -> _Xlen;
    y!BGI_LENGTH -> _Ylen;

    if _neg(x!BGI_SLICES[_Xlen _sub _1]) then
        Bigint_do_negate(x) -> x;
        x!BGI_LENGTH -> _Xlen;
        true -> need_neg;
    endif;

    if _neg(y!BGI_SLICES[_Ylen _sub _1]) then
        Bigint_do_negate(y) -> y;
        y!BGI_LENGTH -> _Ylen;
        not(need_neg) -> need_neg;
    endif;

    ;;; make x the longer of the two -- since the inner loop is done on x,
    ;;; this saves the outer loop being done as many times
    if _Ylen _gr _Xlen then
        x, y -> x -> y;
        _Xlen, _Ylen -> _Xlen -> _Ylen
    endif;

    ;;; get result structure
    Get_bigint(_Xlen _add _Ylen) -> result;

    ;;; NO GC allowed below

    ;;; Zero high slices of result
    result@BGI_SLICES[_Xlen _add _1] -> _Raddr;
    result@BGI_SLICES[_Xlen _add _Ylen] -> _Yaddr;

    while _Raddr <@(SL) _Yaddr do
        _0 -> _Raddr!(SL)++ -> _Raddr;
    endwhile;

    ;;; set up addresses
    result@BGI_SLICES -> _Raddr;
    y@BGI_SLICES -> _Yaddr;
    ;;; Reuse variable despite misleading name...
    _Yaddr@(SL)[_Ylen] -> _Ylen;
    x@BGI_SLICES -> _Xstart;

    ;;; do least sig slice of y by x
    ;;; unsigned multiply x by slice of y into result
    _bgi_mult(_Yaddr!(SL)++ -> _Yaddr, _Xstart, _Xlen, _Raddr);

    ;;; outer loop for rest of y
    while _Yaddr <@(SL) _Ylen do
        _Raddr@(SL)++ -> _Raddr;        ;;; next result position
        ;;; unsigned multiply x by slice of y value and add into result
        _bgi_mult_add(_Yaddr!(SL)++ -> _Yaddr, _Xstart, _Xlen, _Raddr);
    endwhile;

    ;;; end of NO GC section

    Bigint_return(result) -> Get_store() -> result;
    if need_neg then
        Bgi_negate(result);
    else
        result;
    endif;
enddefine;



;;; --- SIMPLE DIVISION BY A SINGLE ------------------------------------------
;;; (see bigint_divide.p for full division)

    ;;; unsigned divide by a single, used for printing
define Bigint_div_single(dd, _dr, quot);
    lvars dd, rm, quot, _rem, _dr;

    _bgi_div(_dr, dd@BGI_SLICES, dd!BGI_LENGTH, quot@BGI_SLICES)
                                    -> _rem;        ;;; the remainder
    Bigint_return(quot) -> Get_store() -> quot;

    if _pint_testovf(_rem) then
        -> rm
    else
        Get_bigint(_1) -> rm;
        _rem -> rm!BGI_SLICES[_0]
    endif;
    rm, quot;
enddefine;


;;; --- BIGINT ARITH OPERATION VECTOR ----------------------------------------

global constant
    Bigint_ops = initv(OP_VEC_LEN);

    Bgi_+           -> subscrv(OP_+, Bigint_ops),
    Bgi_-           -> subscrv(OP_-, Bigint_ops),
    Bgi_*           -> subscrv(OP_*, Bigint_ops),
    ;;; / position has false rather than Ratio_cons, so that
    ;;; ints are not put in working bigints for Ratio_cons
    false           -> subscrv(OP_/, Bigint_ops),
    ;;; procedures that use OP_// on Bigint_ops must appear after the weakref
    weakref[//, Intgr_//] Bigint_//         ;;; see bigint_divide.p
                    -> subscrv(OP_//, Bigint_ops),
    Bgi_negate      -> subscrv(OP_negate, Bigint_ops),
    Bgi_abs         -> subscrv(OP_abs, Bigint_ops),
    procedure(); ->, 0 endprocedure
                    -> subscrv(OP_fracof, Bigint_ops),
    identfn         -> subscrv(OP_intof, Bigint_ops),
    identfn         -> subscrv(OP_round, Bigint_ops),
    procedure(); if Bigint_neg() then -1 else 1 endif endprocedure
                    -> subscrv(OP_sign, Bigint_ops),
    ;


;;; ---------------------------------------------------------------------

    ;;; find the bit number of the least bit in a bigint /= 0
define Bigint_leastbit() -> _n with_nargs 1;
    lvars _ptr, _n, _x;
    ()@BGI_SLICES[_0] -> _ptr;
    _0 -> _n;
    while _zero(_ptr!(-SL)++ -> _ptr ->> _x) do
        _n _add _:SLICE_BITS -> _n
    endwhile;
    until _x _bitst _1 do
        _n _add _1 -> _n;
        ;;; Unsigned shift would be clearer, but even with
        ;;; signed shift we will get correct bit
        _shift(_x, _-1) -> _x
    enduntil;
enddefine;


;;; --- CONVERSION TO FLOATING-POINT -----------------------------------------

    ;;; Ensures that bigint_float.p present only if floats present as well
define Bigint_dfloat(/* bint, _df */) with_nargs 2;
    FLWEAK Bigint_->_dfloat(_0, false)  ;;; see bigint_float.p
enddefine;


;;; --- COMPARISON ---------------------------------------------------------

    ;;; x > (or =) y
define Bigint_>(_or_=, x, y);
    lvars xval, yval, x, y, _xaddr, _yaddr, _xlen, _ylen, _op, _or_=;
    x!BGI_LENGTH -> _xlen;
    y!BGI_LENGTH -> _ylen;

    x@BGI_SLICES[_xlen] -> _xaddr;
    y@BGI_SLICES[_ylen] -> _yaddr;

    _yaddr--!(-SL) -> _yaddr -> yval;
    if _neg(_xaddr--!(-SL) -> _xaddr ->> xval) then
        ;;; x negative
        if _neg(yval) then
            ;;; both negative
            nonop _lt -> _op
        else
            ;;; x negative, y positive
            return(false)
        endif
    elseif _neg(yval) then
        ;;; x positive, y negative
        return(true)
    else
        ;;; both positive
        nonop _gr -> _op
    endif;

    ;;; both same sign
    returnif(_xlen /== _ylen) (_op(_xlen, _ylen));

    ;;; same sign and same length
    x@BGI_SLICES -> _xlen;
    repeat
        if xval /== yval then
            return(xval _gr yval)
        endif;
        quitunless(_xaddr >@(SL) _xlen);
        _xaddr--!(SL) -> _xaddr -> xval;
        _yaddr--!(SL) -> _yaddr -> yval
    endrepeat;

    ;;; equal -- return value of equals allowed
    _or_=
enddefine;

define Eq__Bigint(item, bigint);
    lvars item, bigint;
    if isinteger(item) then
        return(false)
    elseif issimple(item) then
        FLWEAK _pf_dfloat_dec(item, FLWEAK _dfop1)
    else
        ;;; structure
        go_on _pint(item!KEY!K_NUMBER_TYPE) to
        ;;; 1     2     3   4    5        6
            NO  BIGINT  NO  NO  DDEC    COMPLEX
        else NO;

        BIGINT:
            if item == bigint then
                true
            elseif item!BGI_LENGTH == bigint!BGI_LENGTH then
                _CMP(@@(SL)[item!BGI_LENGTH],
                                item@BGI_SLICES, bigint@BGI_SLICES)
            else
                false
            endif;
            return;

        DDEC:
            FLWEAK _pf_dfloat_ddec(item, FLWEAK _dfop1); goto CMPFLOAT;

        COMPLEX:
            if CXWEAK Complex_is_float_real(item, FLWEAK _dfop1) then
                goto CMPFLOAT
            endif;          ;;; else drop thru to NO

        NO:
            if item!KEY!K_FLAGS _bitst _:M_K_MATCH_VAR then
                fast_chain(bigint, item, item!KEY!K_SYS_EQUALS)
            endif;
            return(false)
    endif;

    CMPFLOAT:
    ;;; drop thru for float comparison
    Bigint_dfloat(bigint, FLWEAK _dfop2);
    FLWEAK _pfeq(FLWEAK _dfop1, FLWEAK _dfop2)
enddefine;


;;; --- PRINTING -----------------------------------------------------------

    ;;; For each radix, a number of digits and the corresponding power of
    ;;; the radix, specifying the slice size used for printing bigintegers.
    ;;; These are derived from PRINT_SLICE_BITS, which is the largest number
    ;;; of bits such that both the divisor for obtaining each print slice
    ;;; and the resulting slice (i.e. the remainder) are integers.

lconstant
    bgi_pr_digits   =   procedure;
                            lvars radix, i, log_ps = PRINT_SLICE_BITS*log(2),
                                maxp = 2 ** PRINT_SLICE_BITS;
                            {%  0,
                                for radix from 2 to 36 do
                                    intof(log_ps/log(radix)) -> i;
                                    if radix ** i >= maxp then
                                        i - 1
                                    else i
                                    endif
                                endfor
                            %}
                        endprocedure(),

    bgi_pr_divisors =   procedure;
                            lvars radix;
                            {%  0,
                                for radix from 2 to 36 do
                                    radix ** subscrv(radix, bgi_pr_digits)
                                endfor
                            %}
                        endprocedure();


define lconstant Bgi_print(bint);
    lvars bint;

    define lconstant Do_bgi_print(_quot);
        lvars _rm, _quot, _radix = pop_pr_radix;

        ;;; print an integer with leading zeros in field width _width
        define lconstant Bgi_print_int(i, _width);
            lvars i, digit, _width;
            if i == 0 then
                repeat _width times cucharout(`0`) endrepeat
            else
                i fi_// pop_pr_radix -> i -> digit;
                Bgi_print_int(i, _width fi_- 1);
                cucharout(if digit fi_> 9 then `7` else `0` endif fi_+ digit)
            endif
        enddefine;

        ;;; destructive divide, quotient back into dividend then truncate it
        Bigint_div_single(_quot, _int(bgi_pr_divisors(_radix)), _quot)
                                -> _quot -> _rm;
        if isinteger(_quot) then
            ;;; leftmost part
            Print_int(_quot)            ;;; print with no leading zeros
        else
            ;;; N.B. Because this recursive call will truncate the bigint in
            ;;; _quot to an integer (and free the mem it occupies), _quot
            ;;; must be a nonpop variable in case the _CHECKUSER below
            ;;; causes a GC.
            Do_bgi_print(_quot)     ;;; print leftwards part first
        endif;
        ;;; then remainder with leading zeros
        _CHECKUSER;
        Bgi_print_int(_rm, bgi_pr_digits(_radix))
    enddefine;      /* Do_bgi_print */

    if Bigint_neg(bint) then
        ;;; negative -- put the minus sign out first so that the
        ;;; copy or negated version is at the end of the heap
        cucharout(`-`);
        Bigint_do_negate(bint)
    else
        Bigint_copy(bint)
    endif;
    Do_bgi_print()
enddefine;

;;; -------------------------------------------------------------------------

    /*  Same as -Uint_->_pint- in int_convert.p, but in here to force
        inclusion of bigintegers
    */
define Uint_->_bigint() with_nargs 1;
    Uint_->_pint()
enddefine;


;;; --- BIGINTEGER KEY --------------------------------------------------------


define isbiginteger(item);
    lvars item;
    if iscompound(item) and item!KEY == biginteger_key then true else false endif
enddefine;

define lconstant Hash_bigint(item);
    lvars item _len;
    item!BGI_LENGTH -> _len;
    _pint(item!BGI_SLICES[_0] fi_+ item!BGI_SLICES[_len _sub _1] fi_+ _len)
enddefine;


constant
    biginteger_key = struct KEY_V =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL _biset _:M_K_NONWRITEABLE,
                                ;;; K_FLAGS
        _:GCTYPE_USERNFVEC,     ;;; K_GC_TYPE
        BGI_VEC_GETSIZE,        ;;; K_GET_SIZE

        "biginteger",           ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        isbiginteger,           ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        Eq__Bigint,             ;;; K_SYS_EQUALS
        WREF Eq__Bigint,        ;;; K_EQUALS
        Bgi_print,              ;;; K_SYS_PRINT
        WREF Bgi_print,         ;;; K_PRINT
        WREF Hash_bigint,       ;;; K_HASH

        _:NUMTYPE_BIGINTEGER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_BIGINT,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        _:BGI_VEC_TYPE,         ;;; K_FIELD_CODE_V
        false,                  ;;; K_INIT_V
        false,                  ;;; K_CONS_V
        false,                  ;;; K_DEST_V
        false,                  ;;; K_SUBSCR_V
        false                   ;;; K_FAST_SUBSCR_V
        %};


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 13 1996
        Fixed Bigint_return so that it can safely be given a constant
        nonwriteable bigint.
--- John Gibson, Sep 19 1995
        Changed Bigint_op_bgint_int etc to use new macros
        SAVEWORKBGI/RESTWORKBGI
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, May 25 1993
        Corrected problem in Bgi_* where the result was -ve instead of
        +ve when multiplying two -ve bigints which were both powers of
        2**SLICE_BITS.
--- John Gibson, Dec 13 1990
        Fixed problem in Do_bgi_print (see comment about _quot)
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Dec  6 1989
        Changes for new pop pointers
--- John Gibson, Feb 28 1988
        Procedures into section Sys
--- John Gibson, Feb  9 1988
        Changes for splitting up files, sectioning, putting in weakrefs, etc.
--- John Gibson, Dec 18 1987
        Added some missing declarations at top of file
--- John Williams, Nov  3 1987
        Improved hashing of bigintegers (see BR aarons@tsuna.uucp.22)
--- John Gibson, Sep 15 1987
        Replaced signed bigint slice type sSL with -SL
--- John Gibson, Sep  5 1987
        New key format for -biginteger_key-
--- John Gibson, Aug 14 1987
        Changed for segmented system
 */
