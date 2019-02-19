/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/src/float_util.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;;----------------- FLOATING-POINT UTILITIES ---------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

constant
        procedure (integer_leastbit, integer_length,
        Sys$-Float_->_Int, Sys$-Float_fsign, Sys$-Bigint_dfloat)
    ;


;;; -------------------------------------------------------------------------

section $-Sys => float_decode, float_scale, float_sign, float_code_bytes;

define lconstant Dfloat_dec(item, _df_opnd);
    lvars item, _df_opnd;
    if issimple(item) then
        unless isinteger(item) then
            _pf_dfloat_dec(item, _df_opnd);
            return(_:ARGTYPE_DECIMAL)
        endunless
    elseif item!KEY == ddecimal_key then
        _pf_dfloat_ddec(item, _df_opnd);
        return(_:ARGTYPE_DDECIMAL)
    endif;
    mishap(item, 1, 'DECIMAL OR DDECIMAL NEEDED')
enddefine;

define float_decode(float, _wantint) -> (float, _fexp, _fsign);
    lvars float, _arg = _dfop1, _fexp, _wantint, _argtype, _fsign;
    dlocal popdprecision = true;
    Dfloat_dec(float, _arg) -> _argtype;            ;;; floats into _arg
    if _wantint then
        ;;; all results are integers
        if _pfneg(_arg) then
            _pfnegate(_arg);
            -1
        else
            1
        endif -> _fsign;
        Float_->_Int(_argtype, _arg) -> (float, _fexp)
    else
        Float_fsign(_arg, _argtype) -> _fsign;
        if _pfneg(_arg) then _pfnegate(_arg) endif;
        if _pfzero(_arg) then
            0 -> _fexp
        else
            _pint(_pf_expof(_arg)) -> _fexp;
            _0 -> _pf_expof(_arg) -> ;
            Consdecimal(_argtype, _arg) -> float
        endif
    endif
enddefine;

define float_scale(float, _fexp);
    lvars float, _arg = _dfop1, _fexp, _argtype;
    dlocal popdprecision = true;
    Dfloat_dec(float, _arg) -> _argtype;            ;;; floats into _arg
    Check_integer(_fexp, false);
    if _pfzero(_arg) then
        return(float)
    elseif _pf_expof(_arg) _add _int(_fexp) -> _pf_expof(_arg) then
        Consdecimal(_argtype, _arg)
    else
        false           ;;; return false for overflow/underflow
    endif
enddefine;

define float_sign(float1, float2);
    lvars float1, float2, _arg1 = _dfop1, _type1;
    dlocal popdprecision = true;
    Dfloat_dec(float1, _arg1) -> _type1;
    if float2 then
        lvars _arg2 = _dfop2, _type2;
        Dfloat_dec(float2, _arg2) -> _type2;
        if _pfneg(_arg1) == _pfneg(_arg2) then
            float2
        else
            _pfnegate(_arg2);
            Consdecimal(_type2, _arg2)
        endif
    else
        Float_fsign(_arg1, _type1)
    endif
enddefine;


define float_code_bytes(float) /* -> (b1, ... b_nbytes, nbytes) */;
    lvars n, nbytes = 1, mant, expo, _flags, _arg = _dfop1, _argtype;
    dlocal popdprecision = true;
    _CLAWBACK_SAVE;

    Dfloat_dec(float, _arg) -> _argtype;            ;;; floats into _arg
    if _pfneg(_arg) then
        _pfnegate(_arg);
        _2:100
    else
        _2:000
    endif -> _flags;
    if _argtype == _:ARGTYPE_DDECIMAL then
        _flags _biset _2:001 -> _flags
    endif;

    Float_->_Int(_argtype, _arg) -> (mant, expo);
    if integer_leastbit(mant) ->> n then
        mant >> n -> mant;
        expo fi_+ n -> expo
    endif;
    if expo fi_< 0 then
        -expo -> expo;
        _flags _biset _2:010 -> _flags
    endif;

    define out_n(int) -> n;
        lvars n = (integer_length(int) fi_+ 7) fi_div 8;
        fast_repeat n times
            int && 16:FF;
            int >> 8 -> int
        endrepeat;
    enddefine;

    out_n(mant) -> n;
    nbytes fi_+ n -> nbytes;
    _shift(_int(n), _3) _biset _flags -> _flags;

    out_n(expo) -> n;
    nbytes fi_+ n -> nbytes;
    _shift(_int(n), _6) _biset _flags -> _flags;

    _pint(_flags);
    Clawback(nbytes)
enddefine;
;;;
define updaterof float_code_bytes(_flags, nbytes) /* -> float */;
    lvars _x, _n, _expo, _df = _dfop1;
    dlocal popdprecision = true;

    define in_n_float(_n, _df, _expo);
        lvars nbits, _df, _n, _expo;
        lstackmem dfloat _df_work;
        if _zero(_n) then 0, _1 -> _n endif;
        _n _sub _1 -> _n;
        ;;; float ms slice
        _pf_dfloat_int(_int(), _df);
        _pf_expof(_df) -> nbits;

        until _zero(_n) do
            if _n _greq _3 then
                _shift(_int(), _16) -> _x;
                _shift(_int(), _8) _add _x -> _x;
                _int() _add _x -> _x;
                _n _sub _3 -> _n;
                nbits _add _24
            elseif _n == _2 then
                _shift(_int(), _8) -> _x;
                _int() _add _x -> _x;
                _n _sub _2 -> _n;
                nbits _add _16
            else
                _int() -> _x;
                _n _sub _1 -> _n;
                nbits _add _8
            endif -> _pf_expof(_df) -> ;
            _pf_dfloat_int(_x, _df_work);
            _pfadd(_df, _df_work) -> ;
            _pf_expof(_df) -> nbits;        ;;; re-get in case rounding occurred
            quitif(nbits _greq _:DFLOAT_SIG_BITS)
        enduntil;

        unless _zero(_n) then erasenum(_pint(_n)) endunless;
        unless (_zero(_n) and _zero(_expo))
            or (_shift(_n, _3) _add nbits _add _expo -> _pf_expof(_df))
        then
            ;;; overflowed
            Float_overflow(0, 'converting bytes to float', 'arith-inttodf')
        endunless
    enddefine;

    Check_integer(_flags, 0);
    _int(_flags) -> _flags;

    _shift(_flags, _-6) _bimask _2:11 -> _n;
    if _n == _2 then
        _shift(_int(), _8) -> _expo;
        _int() _add _expo
    elseif _n == _1 then
        _int()
    else
        _0
    endif -> _expo;
    if _flags _bitst _2:010 then _negate(_expo) -> _expo endif;

    _shift(_flags, _-3) _bimask _2:111 -> _n;
    in_n_float(_n, _df, _expo);
    if _flags _bitst _2:100 then _pfnegate(_df) endif;

    Consdecimal(if _flags _bitst _2:001 then _:ARGTYPE_DDECIMAL
                         else _:ARGTYPE_DECIMAL
                         endif, _df)
enddefine;

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul  7 1998
        Added float_code_bytes
--- John Gibson, Apr 13 1992
        Changed -float_decode- so that sign result is an integer when
        _wantint arg is true.
--- John Gibson, Feb  9 1988
        Was floatutil.p
--- John Gibson, Aug 16 1987
        Lconstant'ed etc.
 */
