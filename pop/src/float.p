/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/src/float.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;----------- FLOATING POINT (DECIMALS & DDECIMALS) --------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'gctypes.ph'

global constant
        procedure pr_field,
        _pf_cvt_to_dec, _pf_cvt_to_ddec
    ;

section $-Sys;

constant
        procedure (Bigint_dfloat, Ratio_dfloat, Dfloat_->_bigint)
    ;

endsection;

;;; ------------------------------------------------------------------------

section $-Sys => popdprecision pop_pr_places pop_pr_exponent
                 isdecimal isddecimal decimal_key ddecimal_key;

global vars
    popdprecision   = false,
    pop_pr_places   = 6,
    pop_pr_exponent = false,
    ;


constant
    double_float_0  = 0.0d0,

    _log_2  = _0.6931471805599453,      ;;;  log(2)

    _dfop1      = writeable _0.0d0,
    _dfop2      = writeable _0.0d0,
    _dfresult   = writeable _0.0d0,
    _dfradix    = writeable _0.0d0,
    _dffrac     = writeable _0.0d0,
    ;


;;; --- GENERAL ----------------------------------------------------------

define :inline lconstant _DFVALS(_df);
    _df!(w)[_0], _df!(w)[_1]
enddefine;

lconstant macro _OPND_VALS = [
    (_DFVALS(_dfop1), _DFVALS(_dfop2), _DFVALS(_dfresult),
        _DFVALS(_dfradix), _DFVALS(_dffrac))
    ];

    /*  Used in getstore.p to save operand values across GCs caused by
        getting a ddecimal
    */
define Float_opnd_vals(); _OPND_VALS enddefine;
define updaterof Float_opnd_vals(); -> _OPND_VALS enddefine;


define Float_overflow(n, mess, idstring);
    lvars n, mess, idstring;
    chain((), n, 'FLOATING-POINT OVERFLOW',
                        if mess then sys_>< ' (' sys_>< mess sys_>< ')' endif,
                    if idstring then idstring
                    elseif n == 1 then 'arith-1arg'
                    else 'arith-2arg'
                    endif sys_>< ':arith-fltovf',
        mishap)
enddefine;

    ;;; Float a number operand into a double float operand, returning
    ;;; arg type (or -1 for a complex)
define Dfloat(num, _df_opnd);
    lvars num, _df_opnd;
    if issimple(num) then
        if isinteger(num) then
            _pf_dfloat_int(_int(num), _df_opnd);
            return(_:ARGTYPE_RATIONAL)      ;;; meaning arg was rational
        else
            ;;; decimal
            _pf_dfloat_dec(num, _df_opnd);
            return(_:ARGTYPE_DECIMAL)       ;;; meaning arg was decimal
        endif;
    else
        ;;; structure
        go_on _pint(num!KEY!K_NUMBER_TYPE) to
        ;;;   1       2       3      4       5      6
            ERROR   BIGINT  RATIO   ERROR   DDEC  CMPLX
        else ERROR;

        BIGINT: BGWEAK Bigint_dfloat(num, _df_opnd);
                return(_:ARGTYPE_RATIONAL);

        RATIO:  RTWEAK Ratio_dfloat(num, _df_opnd);
                return(_:ARGTYPE_RATIONAL);

        DDEC:   _pf_dfloat_ddec(num, _df_opnd);
                return(_:ARGTYPE_DDECIMAL);

        CMPLX:  return(_-1);        ;;; return negative for complex

        ERROR:  mishap(num, 1, 'NUMBER NEEDED')
    endif
enddefine;

    ;;; produce a decimal/ddecimal result
define Consdecimal(_df_opnd) -> result with_nargs 2;
    lvars result, _df_opnd;

    define lconstant Single_ovf(_df_opnd);
        dlocal popdprecision = true;
        lvars _df_opnd;
        Float_overflow(Consdecimal(_:ARGTYPE_DDECIMAL, _df_opnd), 1,
                        'converting double to pop decimal', 'arith-dftopd')
    enddefine;

    go_on _pint() to            ;;; _argtype on stack
        ;;;  2:001   2:010   2:011
        ;;; dec-dec rat-rat dec-rat
              DEC     RAT     DEC
    else DDC;   ;;; 2:100, 2:101, 2:110 and 2:111

    DEC:
        ;;; arg(s) were decimal/rational -- mishap if overflow
        returnif(_pf_cvt_to_dec(_df_opnd) ->> result);
        ;;; overflowed
        Single_ovf(_df_opnd);

    DEC_ALLOW_OVF:
        ;;; arg(s) were ddecimal/rational but popdprecision false
        ;;; -- produce double only if overflow
        returnif(_pf_cvt_to_dec(_df_opnd) ->> result);
        goto CONSDDEC;

    RAT:
        ;;; arg(s) were rational -- produce double
        ;;; if popdprecison true but popdprecision /= "ddecimal"
        unless popdprecision then
            goto DEC_ALLOW_OVF
        elseif popdprecision == "ddecimal" then
            goto DEC
        else
            goto CONSDDEC
        endunless;

    DDC:
        ;;; arg(s) were double -- produce double if popdprecision
        unless popdprecision then goto DEC_ALLOW_OVF endunless;
        ;;; drop thru to CONSDDEC

    CONSDDEC:
        ;;; produce new ddecimal record
        Get_store(@@(struct DDECIMAL)++) -> result;
        ddecimal_key -> result!KEY;

        ;;; Move double float parts into ddecimal record.
        ;;; (Also, For IEEE, get rid of -0.0 and denormalised values.)
        _pf_cvt_to_ddec(_df_opnd, result)
enddefine;


;;; --- ARITHMETIC -------------------------------------------------------

define lconstant Flt_intof(_df_opnd) with_nargs 2;  ;;; args are _argtype, _df_opnd
    lvars _df_opnd;
    -> ;    ;;; erase _argtype
    unless _pf_intof(_df_opnd) and _pint_testovf(/* on stack */) then
        ;;; need to produce a bigint
        if testdef biginteger_key then
            chain(_df_opnd, BGWEAK Dfloat_->_bigint)
        else
            mishap(Consdecimal(_:ARGTYPE_DECIMAL, _df_opnd), 1,
                    'FLOAT TO INTEGER OVERFLOW (bigintegers not loaded)')
        endif
    ;;; else popint result is on the stack
    endunless
enddefine;

define Float_qrem(_argtype, _dividend, _divisor);
    lvars _dividend _divisor;   ;;; these are always _dfop1 and _dfop2
    lvars _argtype;
    _pfcopy(_dfresult, _dividend);          ;;; copy dividend to _dfresult
    if _pfdiv(_dividend, _divisor) then
        _pfmodf(_dffrac, _dividend);        ;;; int in _dividend, frac in _dffrac
        _pfmult(_divisor, _dividend) -> ;   ;;; regain integer part of dividend
        _pfsub(_dfresult, _divisor) -> ;    ;;; sub from dividend to get remainder
        Consdecimal(_argtype, _dfresult);   ;;; return floating remainder
        ;;; and return integer quotient, out of Arith_2
        _chainfrom_caller(_argtype, _dividend, Flt_intof)
    else
        ;;; if overflow, return false to Arith_2
        _argtype, false
    endif
enddefine;

define lconstant Flt_negate() with_nargs 2;     ;;; args are _argtype, _df_opnd
    chain(_pfnegate(dup()), Consdecimal)
enddefine;

define lconstant Flt_abs() with_nargs 2;    ;;; args are _argtype, _df_opnd
    chain(_pfabs(dup()), Consdecimal)
enddefine;

define lconstant Flt_fracof(_df_opnd) with_nargs 2; ;;; args are _argtype, _df_opnd
    lvars _df_opnd;
    _pfmodf(_dfop2, _df_opnd);      ;;; frac in _dfop2
    chain(_dfop2, Consdecimal)      ;;; _argtype on stack
enddefine;

define lconstant Flt_round(_df_opnd) with_nargs 2;  ;;; args are _argtype, _df_opnd
    lvars _df_opnd;
    _pfadd(_df_opnd, if _pfneg(_df_opnd) then _-0.5 else _0.5 endif) -> ;
    chain(_df_opnd, Flt_intof)
enddefine;

define Float_fsign(_argtype) with_nargs 2;
    lvars _argtype;
    if _pfneg() then        ;;; _df_opnd on stack
        if _argtype == _:ARGTYPE_DECIMAL then -1.0s0 else -1.0d0 endif
    else
        if _argtype == _:ARGTYPE_DECIMAL then 1.0s0 else 1.0d0 endif
    endif
enddefine;

define lconstant Flt_sign(_argtype, _df_opnd);
    lvars _argtype, _df_opnd;
    unless popdprecision then _:ARGTYPE_DECIMAL -> _argtype endunless;
    if _pfzero(_df_opnd) then
        if _argtype == _:ARGTYPE_DECIMAL then 0.0s0 else 0.0d0 endif
    else
        chain(_df_opnd, _argtype, Float_fsign)
    endif
enddefine;


;;; --- FLOAT ARITH OPERATIONS VECTOR ------------------------------------

constant
    Float_ops = initv(OP_VEC_LEN);

    _pfadd      -> subscrv(OP_+, Float_ops),
    _pfsub      -> subscrv(OP_-, Float_ops),
    _pfmult     -> subscrv(OP_*, Float_ops),
    _pfdiv      -> subscrv(OP_/, Float_ops),
    _pfqrem     -> subscrv(OP_//, Float_ops),   ;;; dummy subroutine that chains -Float_qrem-
    Flt_negate  -> subscrv(OP_negate, Float_ops),
    Flt_abs     -> subscrv(OP_abs, Float_ops),
    Flt_fracof  -> subscrv(OP_fracof, Float_ops),
    Flt_intof   -> subscrv(OP_intof, Float_ops),
    Flt_round   -> subscrv(OP_round, Float_ops),
    Flt_sign    -> subscrv(OP_sign, Float_ops),
    ;


;;; --- CONVERSION TO INTEGER -------------------------------------------

define Float_->_Int(_argtype, _df);
    lvars _df, _argtype, _fexp, _power;
    returnif(_pfzero(_df)) (0, 0);
    _pf_expof(_df) -> _fexp;
    if _argtype == _:ARGTYPE_DECIMAL then
        _:DECIMAL_SIG_BITS
    else
        _:DDECIMAL_SIG_BITS
    endif ->> _power ->  _pf_expof(_df) -> ;
    Flt_intof(_argtype, _df), _pint(_fexp _sub _power)
enddefine;


;;; --- READING FLOATING POINT NUMBERS -----------------------------------

    ;;; Return double float with integer part -int_part- and fractional
    ;;; part given by digits on stack upto <false>. (Used in item.p)
define Assemble_dfloat(int_part, radix);
    lvars int_part, radix;
    dlocal popdprecision = true;
    Dfloat(radix, _dfradix) -> ;        ;;; float radix
    Dfloat(0, _dffrac) -> ;             ;;; total for fraction
    while dup() do
        Dfloat((), _dfop1) -> ;         ;;; float digit
        _pfadd(_dffrac, _dfop1) -> ;    ;;; add digit as integer part
        _pfdiv(_dffrac, _dfradix) ->    ;;; then divide by radix
    endwhile -> ;                       ;;; erase the false
    ;;; add fraction to integer part
    Dfloat(int_part, _dfresult) -> ;
    _pfadd(_dfresult, _dffrac) -> ;
    Consdecimal(_:ARGTYPE_DDECIMAL, _dfresult)
enddefine;

    ;;; produce float operator precedence -- used by identprops
define Float_op_prec(_prec);
    lvars _prec;
    _pf_dfloat_int(_prec, _dfop1);
    _pfdiv(_dfop1, _10.0) -> ;
    Consdecimal(_:ARGTYPE_DECIMAL, _dfop1)
enddefine;

;;; --- PRINTING FLOATING POINT NUMBERS ----------------------------------

    ;;; numbers of significant digits in each radix 2-36

lconstant
    dec_sig_digits  =   procedure;
                            lvars radix, lsig = DECIMAL_SIG_BITS*log(2);
                            cons_with consstring {% 0,
                                for radix from 2 to 36 do
                                    intof(lsig/log(radix) + 0.1)
                                endfor
                            %}
                        endprocedure(),

    ddec_sig_digits =   procedure;
                            lvars radix, lsig = DDECIMAL_SIG_BITS*log(2);
                            cons_with consstring {% 0,
                                for radix from 2 to 36 do
                                    intof(lsig/log(radix) + 0.1)
                                endfor
                            %}
                        endprocedure();


    ;;; working string for printing floating point
lvars
    prf_string = writeable inits(64);

define lconstant Flt_print(num);
    lvars n, num, string, pointdone, power,
        _Nint, _Nsig, _radix, _len, _firstfrac, _fnum = _dfresult,
        _pr_places = pop_pr_places, _pad0,
        ;
    dlocal pop_pr_radix,
            prf_string,     ;;; set false while using work string
        ;

    define lconstant Prf_intdigits();
        lvars digit, count, _fdigit = _dfop1, _fnum = _dfresult,
            _fradix = _dfradix;
        if _pfzero(_fnum) or _pfneg(_fnum) then 0
        else
            _pfdiv(_fnum, _fradix) -> ;
            _pfmodf(_fdigit, _fnum);        ;;; int part in _fnum, frac in _fdigit
            _pfmult(_fdigit, _fradix) -> ;  ;;; get digit as int part
            _pfadd(_fdigit, _0.5) -> ;      ;;; round up
            _pint(_pf_intof(_fdigit) ->) -> digit;  ;;; get digit
            Prf_intdigits() fi_+ 1 -> count;
            (digit), count
        endif;
    enddefine;      /* Prf_intdigits */

    define lconstant Prf_fracdigits(digits_req, _frac_sig);
        lvars digits_req, digit, _frac_sig, _len, _fint = _dfop1,
            _ffrac = _dffrac;
        stacklength() -> _len;
        while digits_req /== 0 do
            _pfmult(_ffrac, _dfradix) -> ;
            _pfcopy(_fint, _ffrac);     ;;; copy to _fint
            _pfmodf(_ffrac, _fint);     ;;; int in _fint, frac in _ffrac
            _pint(_pf_intof(_fint) -> /* erase true */) -> digit;
            if digit /== 0 and _frac_sig then
                ;;; _frac_sig is the max number of digits after first non-zero
                fi_min(_frac_sig, digits_req) -> digits_req;
                false -> _frac_sig;
            endif;
            (digit);
            digits_req fi_- 1 -> digits_req;
        endwhile;
        stacklength() fi_- _len
    enddefine;      /* Prf_fracdigits */

    define lconstant Prf_rnd_and_store(_numdig, _radix) -> string -> _carry;
        lvars digit, string, _numdig, _carry, _radix;
        if prf_string and _numdig fi_<= 64 then
            prf_string;
            false -> prf_string         ;;; set false in case of recursion
        else
            inits(_numdig)
        endif -> string;
        ;;; round the first digit off the stack
        () fi_+ (_radix fi_div 2) fi_>= _radix -> _carry;
        until _numdig == 1 do
            () -> digit;
            if _carry then
                digit fi_+ 1 -> digit;
                false -> _carry;
                if digit fi_>= _radix then
                    true -> _carry;
                    digit fi_- _radix -> digit
                endif;
            endif;
            if digit fi_> 9 then `7` else `0` endif fi_+ digit
                    -> fast_subscrs(_numdig, string);
            _numdig fi_- 1 -> _numdig;
        enduntil;
        ;;; if carry to leading digit, put it out
        ;;; digits then begin at subscript 2
        if _carry then cucharout(`1`) endif;
    enddefine;      /* Prf_rnd_and_store */

    define lconstant Prf_get_power();
        lvars expo, power = _0, _fradix = _dfop1, _fnum = _dfresult, _radexpo
            ;
        if _pfzero(_fnum) then return(0) endif;
        if pop_pr_radix == 2 then
            _pint(_pf_expof(_fnum) _sub _1) -> power;
            _1 -> _pf_expof(_fnum) -> ;
            return(power)
        endif;
        _pf_expof(_dfradix) -> _radexpo;
        repeat
            _pf_expof(_fnum) _div _radexpo -> expo -> ;
            quitif(_zero(expo));
            expo _add power -> power;
            if _neg(expo) then
                _pfcopy(_fradix, _dfradix), _negate(expo) -> expo;
            else
                _pfcopy(_fradix, _1.0), _pfdiv(_fradix, _dfradix) -> ;
            endif;
            repeat
                if expo _bitst _1 then _pfmult(_fnum, _fradix) -> endif;
                quitif(_zero(_shift(expo, _-1) ->> expo));
                _pfmult(_fradix, _fradix) -> ;
            endrepeat;
        endrepeat;
        while _pfsgreq(_fnum, _dfradix) do
            _pfdiv(_fnum, _dfradix) -> , power _add _1 -> power
        endwhile;
        while _pf_expof(_fnum) _slt _1 do
            _pfmult(_fnum, _dfradix) -> , power _sub _1 -> power
        endwhile;
        _pint(power)
    enddefine;      /* Prf_get_power */

    ;;; get current radix
    pop_pr_radix -> _radix;

    (num, _fnum);
    if issimple(num) then _pf_dfloat_dec() else _pf_dfloat_ddec() endif;

    if _pfneg(_fnum) then
        ;;; negative - insert - sign and take abs value
        cucharout(`-`);
        ;;; refloat in case of recursion
        (num, _fnum);
        if issimple(num) then _pf_dfloat_dec() else _pf_dfloat_ddec() endif;
        _pfabs(_fnum)
    endif;

    _pf_dfloat_int(_int(_radix), _dfradix);
    false -> power;
    if pop_pr_exponent then
        ;;; print in exponent format
        Prf_get_power() -> power
    endif;

    ;;; get integer part in _fnum, frac in _dffrac
    _pfmodf(_dffrac, _fnum);
    ;;; put the digits of the integer part on the stack
    Prf_intdigits() -> _Nint;
    ;;; get desired number of fractional places
    unless isinteger(_pr_places) and _pr_places fi_>= 0 then
        6 -> _pr_places
    endunless;
    _pr_places fi_>> 16 -> _pad0;
    _pr_places fi_&& 16:FFFF -> _pr_places;

    ;;; get the max number of significant digits for the radix
    fast_subscrs(_radix, if issimple(num) then dec_sig_digits
                         else ddec_sig_digits
                         endif) -> _Nsig;
    _pr_places == 0 -> pointdone;
    if _Nint fi_> _Nsig then
        ;;; more integer digits than significant digits
        ;;; no frac digits needed
        erasenum(_Nint fi_- _Nsig fi_- 1);  ;;; clear insig digits but one
        _Nsig fi_+ 1 -> _len;
        Prf_rnd_and_store(_len, _radix) -> string -> ;
        fast_for n from 2 to _len do
            cucharout(fast_subscrs(n, string))
        endfast_for;
        ;;; output zeros for power places
        repeat _Nint fi_- _Nsig times cucharout(`0`) endrepeat;
    else
        if _Nint /== 0 then
            ;;; number is >= 1 and will need at least one frac digit for rounding
            (fi_min(_pr_places, _Nsig fi_- _Nint) fi_+ 1, false);
        else
            ;;; number is less than 1
            1 -> _Nint;
            (0);                        ;;; for extra integer place
            (_pr_places fi_+ 1, _Nsig fi_+ 1);
        endif;
        Prf_fracdigits() fi_+ _Nint -> _len;
        _Nint fi_+ 2 -> _firstfrac;     ;;; position of first frac dig
        if ((Prf_rnd_and_store(_len, _radix) -> string) or _Nint == 2)
        and power then
            ;;; carry from integer part and printing exponent
            ;;; shift point 1 place to left
            _firstfrac fi_- 1 -> _firstfrac;
            _len fi_- 1 -> _len;
            power fi_+ 1 -> power
        endif;
        ;;; erase trailing zeros
        while _len fi_> _firstfrac and fast_subscrs(_len, string) == `0` do
            _len fi_- 1 -> _len
        endwhile;
        ;;; output digits
        fast_for n from 2 to _len do
            if n == _firstfrac then
                if pointdone then quitloop
                else cucharout(`.`), true -> pointdone
                endif
            endif;
            cucharout(fast_subscrs(n, string));
            if pointdone then _pr_places fi_- 1 -> _pr_places endif
        endfast_for
    endif;
    unless pointdone then
        cucharout(`.`), cucharout(`0`);
        _pr_places fi_- 1 -> _pr_places
    endunless;
    if _pad0 /== 0 then
        repeat _pr_places times cucharout(_pad0) endrepeat
    endif;

    if power then
        cucharout(`e`);
        if power fi_>= 0 then cucharout(`+`) endif;
        10 -> pop_pr_radix;
        if isinteger(pop_pr_exponent) then
            if (pop_pr_exponent fi_>> 16 ->> _pad0) == 0 then
                `0` -> _pad0
            endif;
            ;;; right align field with left padding to width
            pr_field(power, pop_pr_exponent fi_&& 16:FFFF, _pad0,
                                                        false, sys_syspr)
        else
            sys_syspr(power)
        endif
    endif
enddefine;      /* Flt_print */


;;; --- DECIMAL AND DDECIMAL KEYS ------------------------------------------

define isdecimal(item);
    lvars item;
    if iscompound(item) then
        item!KEY == ddecimal_key
    else
        not(isinteger(item))
    endif
enddefine;

define isddecimal(item);
    lvars item;
    iscompound(item) and item!KEY == ddecimal_key
enddefine;

define Eq__Float(item, decimal);
    lvars item, decimal;
    if issimple(item) then
        if isinteger(item) then
            _pf_dfloat_int(_int(item), _dfop1)
        else
            ;;; decimal
            _pf_dfloat_dec(item, _dfop1)
        endif
    else
        ;;; structure
        go_on _pint(item!KEY!K_NUMBER_TYPE) to
        ;;; 1     2       3     4    5        6
            NO  BIGINT  RATIO   NO  DDEC    COMPLEX
        else NO;

        BIGINT:
            BGWEAK Bigint_dfloat(item, _dfop1);   goto CMPFLOAT;

        RATIO:
            RTWEAK Ratio_dfloat(item, _dfop1);    goto CMPFLOAT;

        DDEC:
            _pf_dfloat_ddec(item, _dfop1); goto CMPFLOAT;

        COMPLEX:
            if CXWEAK Complex_is_float_real(item, _dfop1) then
                goto CMPFLOAT
            endif;          ;;; else drop thru to NO

        NO:
            if item!KEY!K_FLAGS _bitst _:M_K_MATCH_VAR then
                fast_chain(decimal, item, item!KEY!K_SYS_EQUALS)
            else
                return(false)
            endif
    endif;

    CMPFLOAT:
    if issimple(decimal) then
        _pf_dfloat_dec(decimal, _dfop2)
    else
        _pf_dfloat_ddec(decimal, _dfop2)
    endif;
    _pfeq(_dfop1, _dfop2)
enddefine;


define lconstant Flt_hash() with_nargs 1;
    if issimple(dup()) then
        _pf_dfloat_dec((), _dfop1)
    else
        _pf_dfloat_ddec((), _dfop1)
    endif;
    returnif(_pfzero(_dfop1)) (0);
    _0 -> _pf_expof(_dfop1) -> ;
    _dfop1, _0.5, if _pfneg(_dfop1) then _pfadd() else _pfsub() endif -> ;
    _pf_expof(_dfop1) _add (_int(POPINT_BITS) _add _1) -> _pf_expof(_dfop1) ->;
    if _pf_intof(_dfop1) then _pint() else 0 endif
enddefine;


constant

    decimal_key = struct KEY =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL,          ;;; K_FLAGS
        _:GCTYPE_NONE,          ;;; K_GC_TYPE
        0,                      ;;; K_GET_SIZE

        "decimal",              ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        isdecimal,              ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        Eq__Float,              ;;; K_SYS_EQUALS
        WREF Eq__Float,         ;;; K_EQUALS
        Flt_print,              ;;; K_SYS_PRINT
        WREF Flt_print,         ;;; K_PRINT
        WREF Flt_hash,          ;;; K_HASH

        _:NUMTYPE_DECIMAL,      ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_SIMPLE,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE
        %},

    ddecimal_key = struct KEY_R =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL_RECORD _biset _:M_K_NONWRITEABLE,
                                ;;; K_FLAGS
        _:GCTYPE_DDECIMAL,      ;;; K_GC_TYPE
#_IF ##(w)[_1|struct DDECIMAL] = _2
        Rec1_getsize,           ;;; K_GET_SIZE
#_ELSE
        Rec2_getsize,           ;;; K_GET_SIZE
#_ENDIF

        "ddecimal",             ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        isddecimal,             ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        Eq__Float,              ;;; K_SYS_EQUALS
        WREF Eq__Float,         ;;; K_EQUALS
        Flt_print,              ;;; K_SYS_PRINT
        WREF Flt_print,         ;;; K_PRINT
        WREF Flt_hash,          ;;; K_HASH

        _:NUMTYPE_DDECIMAL,     ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_DDEC,     ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct DDECIMAL)++,  ;;; K_RECSIZE_R
        false,                  ;;; K_CONS_R
        false,                  ;;; K_DEST_R
        false,                  ;;; K_ACCESS_R
        %};

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 10 1998
        Removed single*_float_0
--- John Gibson, Apr  1 1996
        Added idstring for Float_overflow
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Nov 30 1993
        Changed CONS label to CONSDDEC so it doesn't autoload CONS in VMS
--- John Gibson, Sep  4 1992
        Made extern type of decimal_key be EXTERN_TYPE_SIMPLE
--- John Gibson, Jan  2 1992
        Replaced M_K_S*IMPLE flag in key with M_K_SPECIAL
--- John Gibson, Oct 17 1991
        Added -Float_opnd_vals-
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Dec  4 1989
        Changes for new pop pointers
--- John Gibson, Aug 12 1988
        Used chaining of procedure where possible
--- John Gibson, Feb 28 1988
        Added -Float_op_prec-
--- John Gibson, Feb  9 1988
        Changes for splitting up files, sectioning, putting in weakrefs, etc.
--- John Gibson, Jan 21 1988
        Moved in -Float_overflow- from errors.p
--- John Gibson, Sep 21 1987
        Replaced explicit moving of _df_opnd fields into result ddecimal
        in -Consdecimal- with call to new subroutine _pf_cvt_to_ddec (which
        also handles IEEE normalisation).
--- John Gibson, Sep  4 1987
        New key formats for decimal_key and ddecimal_key
--- John Gibson, Jul 28 1987
        -pop_pr_exponent- can now be an integer to specify width and padding
        character for exponent field
 */
