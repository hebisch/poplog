/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/src/bigint_float.p
 > Purpose:
 > Author:          John Gibson, Jan 24 1988 (see revisions)
 */

;;; ---------------- BIGINT / FLOATING-POINT CONVERSION ----------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'



;;; --- FLOAT -> BIGINT ---------------------------------------------------

section $-Sys;

    /*  Return double float operand ____df as a bigint
    */
define Dfloat_->_bigint(_df);
    lvars   result, _Raddr, _df, _nbits, _ms, _nslices, _Rstart, _Rlim,
            _s0, _s1;
    lconstant _df_1 = writeable _1.0;       ;;; exponent gets assigned
    lstackmem dfloat _df_work;

    _pf_expof(_df) -> _nbits;
    _nbits _div _:SLICE_BITS -> _nslices -> _nbits;
    if _zero(_nbits) then
        _:SLICE_BITS -> _nbits
    else
        _nslices _add _1 -> _nslices
    endif;

    ;;; get value for ms (sign) slice
    _nbits -> _pf_expof(_df) -> ;       ;;; nbits of integer
    _pf_intof(_df) -> -> _ms;
    if _neg(_ms) then
        ;;; negative -- add 2**_____nbits
        _nbits _add _1 -> _pf_expof(_df_1) -> ;
        _pfadd(_df, _df_1) -> ;
        (_pf_intof(_df) ->) _add _shift(_-1, _nbits) -> _ms;
        if _ms == _-1 then _nslices _sub _1 -> _nslices endif
    endif;

    _df!(w)[_0] -> _s0;  _df!(w)[_1] -> _s1;    ;;; save _df
    Get_bigint(_nslices) -> result;
    _s0 -> _df!(w)[_0];  _s1 -> _df!(w)[_1];    ;;; restore _df

    result@BGI_SLICES -> _Rstart;
    _Rstart@(SL)[_nslices] ->> _Raddr -> _Rlim;

    if _ms /== _-1 then _ms -> _Raddr--!(SL) -> _Raddr endif;

    while _Raddr >@(SL) _Rstart do
        _pfmodf(_df_work, _df);     ;;; frac into work
        _pfcopy(_df, _df_work);     ;;; frac back into _df
        quitif(_pfzero(_df));       ;;; done if no more left
        _pf_expof(_df) _add _:SLICE_BITS -> _pf_expof(_df) -> ; ;;; shift left
        _pf_intof(_df) -> -> _Raddr--!(SL) -> _Raddr
    endwhile;

    while _Raddr >@(SL) _Rstart do _0 -> _Raddr--!(SL) -> _Raddr endwhile;
    if _ms == _-1 then
        ;;; set sign bit on ms slice
        (_Rlim--!(SL) -> _Rlim) _biset _:SIGN_MASK -> _Rlim!(SL)
    endif;

    result
enddefine;


;;; --- BIGINT -> FLOAT --------------------------------------------------

    /*  Float ____bint into double float operand ____df, with ____________adjust_exp added
        to exponent. Return true/false for no overflow/overflow if
        _____________want_result is true, else mishap on overflow
    */
define Bigint_->_dfloat(bint, _df, _adjust_exp, _want_result);
    lvars nbits, bint, _addr, _df, _len, _adjust_exp, _want_result;
    lstackmem dfloat _df_work;

    bint!BGI_LENGTH _sub _1 -> _len;
    bint@BGI_SLICES[_len] -> _addr;

    ;;; float sign slice
    _pf_dfloat_int(_addr!(-SL), _df);
    _pf_expof(_df) -> nbits;

    until _zero(_len) do
        nbits _add _:SLICE_BITS -> _pf_expof(_df) -> ;
        _pf_dfloat_int(_addr--!(SL) -> _addr, _df_work);
        _pfadd(_df, _df_work) -> ;
        _pf_expof(_df) -> nbits;        ;;; re-get in case rounding occurred
        _len _sub _1 -> _len;
        quitif(nbits _greq _:DFLOAT_SIG_BITS)
    enduntil;

    if (_zero(_len) and _zero(_adjust_exp))
    or ((_len _mult _:SLICE_BITS) _add nbits _add _adjust_exp -> _pf_expof(_df))
    then
        ;;; ok
        if _want_result then true endif
    else
        ;;; overflowed
        if _want_result then
            false
        else
            Float_overflow(bint, 1, 'converting biginteger', 'arith-inttodf')
        endif
    endif
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun  2 1994
        Changed to use lstackmem
 */
