/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/src/bigint_shift.p
 > Purpose:
 > Author:          John Gibson, Feb  2 1988 (see revisions)
 */

;;; --------------- BIGINTEGER SHIFT OPERATIONS -------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

;;; -------------------------------------------------------------------------

section $-Sys;

    ;;; shift a bigint left by _nbits
define Bigint_<<(x, _nbits);
    lvars   left_carry, x, result, _Raddr, _Xaddr, _Xlim
            _nbits, _rembits, _rem_mask, _Rlim, _nslices
        ;

    returnif(_zero(_nbits)) (x);
    _nbits _div _:SLICE_BITS -> _nslices -> _nbits;

    x!BGI_LENGTH -> _Xlim;
    _Xlim _add _nslices -> _Rlim;
    if _nbits _sgr _0 then
        _Rlim _add _1 -> _Rlim;   ;;; 1 extra for bit shift overflow
    endif;
    Get_bigint(_Rlim) -> result;

    ;;; zero bottom _nslices of result
    result@BGI_SLICES -> _Raddr;
    _Raddr@(SL)[_nslices] -> _Rlim;
    while _Raddr <@(SL) _Rlim do
        _0 -> _Raddr!(SL)++ -> _Raddr
    endwhile;

    ;;; move from right to left while doing the bit shift
    x@BGI_SLICES -> _Xaddr;
    _Xaddr@(SL)[_Xlim] -> _Xlim;

    if _zero(_nbits) then
        while _Xaddr <@(SL) _Xlim do
            _Xaddr!(SL)++ -> _Xaddr -> _Raddr!(SL)++ -> _Raddr;
        endwhile;
        return(Bigint_return(result) -> Get_store());
    endif;

    _nbits _sub _:SLICE_BITS -> _rembits;   ;;; right shift for carry

    _shift(_1, _nbits) _sub _1 -> _rem_mask;
    _0 -> left_carry;                       ;;; initial left carry is 0

    repeat
        _Xaddr!(SL)++ -> _Xaddr -> x;
        _shift(x, _nbits) _biset left_carry
                                -> _Raddr!(SL)++ -> _Raddr;
        quitunless(_Xaddr <@(SL) _Xlim);
        ;;; right shift for next carry
        _shift(x, _rembits) _bimask _rem_mask -> left_carry;
    endrepeat;

    ;;; final carry with sign
    _shift(x, _rembits) -> _Raddr!(-SL);

    Bigint_return(result) -> Get_store()
enddefine;

    ;;; shift a bigint right by _nbits into result (or create result)
define Bigint_>>_into(x, _nbits, result);
    lvars   right_carry, x, result, _Raddr, _Xaddr, _Rlim
            _nbits, _rembits, _rem_mask, _nslices, _org_result = result;
        ;

    ;;; working bigint of 2 slices
    lconstant work_bgi2 = writeable
                    struct BIGINT =>> {%_2, biginteger_key, =>> {%_0,_0%}%};

    if _zero(_nbits) then
        ;;; copy it to result bigint if specified
        returnunless(result) (x);
        _moveq(@@V_WORDS[x!BGI_LENGTH | SL.r] _sub @@POPBASE,
                                            x@POPBASE, result@POPBASE) -> ;
        return(result)
    endif;
    _nbits _div _:SLICE_BITS -> _nslices -> _nbits;

    x!BGI_LENGTH _sub _nslices -> _Rlim;
    if _Rlim _slt _1 then
        return(if Bigint_neg(x) then -1 else 0 endif)
    elseif _Rlim _slteq _2 and not(result) then
        ;;; 1 or 2 -- use work bigint
        work_bgi2 -> result
    endif;

    if result then
        _Rlim -> result!BGI_LENGTH
    else
        Get_bigint(_Rlim) -> result
    endif;

    ;;; move from left to right while doing the bit shift
    x@BGI_SLICES[x!BGI_LENGTH] -> _Xaddr;

    result@BGI_SLICES[_Rlim] -> _Raddr;
    result@BGI_SLICES[_0] -> _Rlim;

    _:SLICE_BITS _sub _nbits -> _rembits;   ;;; left shift to get carry
    _negate(_nbits) -> _nbits;              ;;; right shift to get remainder

    _shift(_1, _rembits) _sub _1 -> _rem_mask;

    _Xaddr--!(-SL) -> _Xaddr -> x;          ;;; ms part with sign
    _shift(x, _nbits) -> _Raddr--!(-SL) -> _Raddr;  ;;; sign of result
    while _Raddr >@(SL) _Rlim do
        ;;; left shift for carry
        _shift(x, _rembits) -> right_carry;
        _Xaddr--!(SL) -> _Xaddr -> x;
        ;;; right shift for remainder
        (_shift(x, _nbits) _bimask _rem_mask) _biset
                             right_carry -> _Raddr--!(SL) -> _Raddr
    endwhile;

    Bigint_return(result);  ;;; -> (result value, lim)
    if _org_result then
        ->
    elseif result == work_bgi2 then
        result!BGI_LENGTH -> _nslices;
        _2 -> result!BGI_LENGTH;    ;;; MUST return this to 2!
        if () /== result then
            ;;; value is work_bgi2, not an integer, so copy it
            ;;; (chain out so as to clear pop lvars used for nonpop!)
            chain((), _nslices, Bigint_copy_len)
        endif
    else
        ;;; return truncated mem
        -> Get_store()
    endif
enddefine;

constant procedure
    Bigint_>>    = Bigint_>>_into(%false%);


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 28 1992
        Made Bigint_>>_into use a working bigint to avoid creating garbage
        when result is an integer
--- John Gibson, Dec  6 1989
        Changes for new pop pointers
 */
