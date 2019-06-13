/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/src/bigint_float.p
 > Purpose:
 > Author:          John Gibson, Jan 24 1988 (see revisions)
 */

;;; ---------------- BIGINT / FLOATING-POINT CONVERSION ----------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

global constant
    _pf_dfloat_uint, _pf_uintof,
;

;;; --- FLOAT -> BIGINT ---------------------------------------------------

section $-Sys;

    /*  Return double float operand _df as a bigint
    */
define Dfloat_->_bigint(_df);
    lvars   result, _Raddr, _df, _nbits, _ms, _nslices, _Rstart, _Rlim,
            _s0, _s1;
    lconstant _df_1 = writeable _1.0;       ;;; exponent gets assigned
    lstackmem dfloat _df_work;

    _pf_expof(_df) -> _nbits;
    _nbits _div _:SLICE_BITS -> _nslices -> _nbits;
    _nslices _add _1 -> _nslices;

    _nbits -> _pf_expof(_df) -> ;

    ;;; Change to two-complement form
    if _pfneg(_df) then
        _nbits _add _1 -> _pf_expof(_df_1) -> ;
        _pfadd(_df, _df_1) -> ;
        _-1;
    else
        _0;
    endif -> _s0;

    if _zero(_nbits) then
        if _s0 == _-1 then
            ;;; mishap(0, 'Uniimplemented');
            _pf_expof(_df) _add _:SLICE_BITS -> _pf_expof(_df) -> ;
            _pf_uintof(_df) -> -> _ms;
            if _ms == _-1 then
                _nslices _sub _1 -> _nslices;
            else
                _-1 -> _ms;
                _pf_expof(_df) _sub _:SLICE_BITS -> _pf_expof(_df) ->
            endif;
        else
            _0 -> _ms;
        endif;
    else
        _pf_uintof(_df) -> -> _ms;
        _pf_dfloat_uint(_ms, _df_work);
        _pfsub(_df, _df_work) -> ;
        if _s0 == _-1 then
            _ms _add _shift(_-1, _nbits) -> _ms;
        endif;
    endif;

    _df!(w)[_0] -> _s0;  _df!(w)[_1] -> _s1;    ;;; save _df
    Get_bigint(_nslices) -> result;
    _s0 -> _df!(w)[_0];  _s1 -> _df!(w)[_1];    ;;; restore _df

    result@BGI_SLICES -> _Rstart;
    _Rstart@(SL)[_nslices] ->> _Raddr -> _Rlim;

    _ms -> _Raddr--!(SL) -> _Raddr;

    while _Raddr >@(SL) _Rstart do
        quitif(_pfzero(_df));       ;;; done if no more left
        _pf_expof(_df) _add _:SLICE_BITS -> _pf_expof(_df) -> ; ;;; shift left
        _pf_uintof(_df) -> ->> _ms -> _Raddr--!(SL) -> _Raddr;
        _pf_dfloat_uint(_ms, _df_work);
        _pfsub(_df, _df_work) -> ;
    endwhile;

    while _Raddr >@(SL) _Rstart do _0 -> _Raddr--!(SL) -> _Raddr endwhile;
    result
enddefine;


;;; --- BIGINT -> FLOAT --------------------------------------------------

    /*  Float bint into double float operand _df, with _adjust_exp added
        to exponent. Return true/false for no overflow/overflow if
        _want_result is true, else mishap on overflow
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
        _pf_dfloat_uint(_addr--!(SL) -> _addr, _df_work);
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
