/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/src/bigint_divide.p
 > Purpose:
 > Author:          John Gibson, Feb  2 1988 (see revisions)
 */

;;; ------------------ BIGINTEGER DIVISION --------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

global constant
        _bgi_add, _bgi_sub, _bgi_lshift, _bgi_rshiftl,
        _bgi_mult_add, _bgi_sub_mult, _bgi_div,
        _quotient_estimate_init, _quotient_estimate,
    ;

section $-Sys;

constant
        procedure (
        Bigint_negate_range, Bigint_do_negate
        )
    ;

endsection;


;;; ----------------------------------------------------------------------

section $-Sys;

lconstant macro (
    MOST_POSITIVE       = SLICE_MASK,
    MOST_POSITIVE_BIT   = 2 ** (SLICE_BITS-1),
    );

struct LONG_DIVIDE_HELPER {
    dfloat LONG_DIVIDE_HELPER_FLOAT;
    ;;; FIXME: popc allocates only 4 bytes for dfloat,
    ;;; so we add padding to have enough space...
    int LONG_DIVIDE_HELPER_PAD1;
    int LONG_DIVIDE_HELPER_DIV_HI;
};

define lconstant Long_divide(dividend, divisor, _negrem, _want_quot);
    lvars quot, rm, dividend, divisor,
        _Dlim, _DRlen, _Daddr, _DRstart, _DRoff, _Dstart,
        _Qlim, _nextd, _fact, _negrem, _want_quot
    ;
    lstackmem struct LONG_DIVIDE_HELPER _ldh;

    dividend@BGI_SLICES -> _Dstart;
    dividend!BGI_LENGTH -> _Dlim;
    divisor@BGI_SLICES -> _DRstart;
    divisor!BGI_LENGTH -> _DRlen;

    if _DRstart!(SL)[_DRlen _sub _1] == _0 then
        _DRlen _sub _1 -> _DRlen;
    endif;

    if _DRstart!(SL)[_DRlen _sub _1] == _0 then
        mishap(0, 'Unexpexted divisor in Long_divide');
    endif;

    if _want_quot then
        Get_bigint(_Dlim _sub _DRlen) ->> quot; ;;; leave on stack as result
        quot@BGI_SLICES[quot!BGI_LENGTH] -> _Qlim
    endif;

    ;;; To get good estimates of quotient we want higest bit in
    ;;; divisor to be 1.  So we shift both divisor and dividend.
    ;;; This leaves the quotient unaffected, but we need to shift back
    ;;; remainder.
    ;;; quotient_estimate_init computes needed shift amount and
    ;;; stores in _ldh informatiation to quickly compute quotients.
    ;;; With 32-bit slices it is enough to store in _ldh dfloat
    ;;; approximation to inverse of divisor.  With 64-bit slices
    ;;; it gets more hairy...
    _quotient_estimate_init(_DRstart, _DRlen, _ldh) -> _fact;
    if _fact _sgr _0 then
        ;;; shift both
        _bgi_lshift(_Dstart, _Dlim, _fact, _Dstart) -> ;
        _bgi_lshift(_DRstart, _DRlen, _fact, _DRstart) -> ;
    endif;

    _Dstart@(SL)[_Dlim] -> _Dlim;
    _DRstart@(SL)[_DRlen] _sub _DRstart -> _DRoff;

    repeat
        _Dlim--!(SL) -> _Dlim -> _nextd;    ;;; next dividend slice
        _quotient_estimate(_nextd, _Dlim!(SL)[_-1], _ldh) -> quot;

        ;;; subtract quot times divisor from dividend
        _Dlim@(SL)-{_DRoff} -> _Daddr;  ;;; where divisor is aligned to dividend
        unless _zero(quot) then
            ;;; subtract mult by quot from dividend
            _bgi_sub_mult(quot, _DRstart, _DRlen, _Daddr);
            ;;; if needed correct, adding back/subtraction back divisor
            ;;; and changing quot
            _Dlim!(-SL) -> _nextd;
            if _neg(_nextd) then
                _bgi_mult_add(_1, _DRstart, _DRlen, _Daddr);
                quot _sub _1 -> quot
            endif;
            unless _zero(_Dlim!(SL)) then
                mishap(0, 'SYSTEM ERROR IN BIGINT DIVISION')
            endunless;
        endunless;
        ;;; insert quotient slice
        if _want_quot then
            quot -> _Qlim--!(SL) -> _Qlim
        endif;
        quitunless(_Daddr >@(SL) _Dstart)
    endrepeat;

    ;;; truncate 0 slices at top of remainder
    _Dlim -> _Daddr;
    while _zero(_Dlim--!(SL) -> _Dlim) and _Dlim >@(SL) _Dstart do
    endwhile;
    if _neg(_Dlim!(-SL)) then
        _Dlim@(SL)++ -> _Dlim;
    endif;
    _Dlim@(SL)++ -> _Dlim;
    ##(SL){_Dlim, _Dstart} -> _Dlim;

    ;;; finally correct the remainder, shifting it by _fact
    if _fact _sgr _0 then
        _bgi_rshiftl(_Dstart, _Dlim, _fact, _Dstart);
    endif;
    ;;; correct sign of remainder
    if _negrem then
        Bigint_negate_range(_Dstart, _Dlim, _Dstart) ;;; can't overflow
    endif;

    ;;; return length of remainder
    _Dlim;
enddefine;

define lconstant Bgi_qrem(dd, dr, _want_quot);
    lvars dd, dr, quot, rm, org_dd,
        _ddlen, _drlen, _x, _addr, _negquot, _negrem, _want_quot;
    dd!BGI_LENGTH -> _ddlen;
    dr!BGI_LENGTH -> _drlen;

    if _drlen == _1 then
        ;;; divide by a single
        dr!BGI_SLICES[_0] -> _x;
        if _x == _-1 then
            return(0,   if _want_quot then
                            Bigint_return(Bigint_do_negate(dd)) -> Get_store();
                        endif)
        elseif _x == _1 then
            return(0, if _want_quot then Bigint_return(dd) -> endif)
        endif;
        if _neg(dd!BGI_SLICES[_ddlen _sub _1]) then
            Bigint_do_negate(dd) ->> dd -> quot;
            dd!BGI_LENGTH -> _ddlen;
            true ->> _negrem -> _negquot;
        else
            Get_bigint(_ddlen) -> quot;
            false ->> _negrem -> _negquot;
        endif;
        if _neg(_x) then
            _negate(_x) -> _x;
            not(_negquot) -> _negquot;
        endif;
        _bgi_div(_x, dd, _ddlen, quot) -> _x;
        if _negrem then
            _negate(_x) -> _x;
        endif;
        if _want_quot then
            if _negquot then
                Bigint_negate_range(quot, _ddlen, quot);
            endif;
            Bigint_return(quot) -> Get_store() -> quot;
        else
            quot -> Get_store()
        endif;
        if _pint_testovf(_x) then
            -> rm
        else
            Get_bigint(_1) -> rm;
            _x -> rm!BGI_SLICES[_0]  
        endif;
        return(rm, if _want_quot then quot endif);
    elseif _drlen _sub _1 _gr _ddlen then
        ;;; remainder=dividend, quotient=0
        return(Bigint_return(dd) ->, if _want_quot then 0 endif)
    endif;

    ;;; now need positive copies of both
    _CLAWBACK_SAVE;
    dd -> org_dd;                   ;;; save orginal dividend
    dd!BGI_LENGTH _add _1 -> _ddlen;
    if Bigint_neg(dd) then          ;;; dividend with extra 0 slice
        Get_bigint(_ddlen _add _1) -> dd;       ;;; allow 1 more for overflow
        org_dd@BGI_SLICES -> _addr;
        Bigint_negate_range(_addr, _ddlen _sub _1, dd@BGI_SLICES);
        if dd!BGI_SLICES[_ddlen _sub _1] /== _0 then
            ;;; overflowed, extend by 0
            _0 -> dd!BGI_SLICES[_ddlen];
            _ddlen _add _1 -> _ddlen
        else
            ;;; didn't overflow, can truncate
            _ddlen -> dd!BGI_LENGTH;
            dd@V_WORDS[_ddlen|SL.r]@~POPBASE -> Get_store()
        endif;
        true ->> _negrem -> _negquot
    else
        Bigint_copy_len(dd, _ddlen) -> dd;
        false ->> _negrem -> _negquot
    endif;

    if Bigint_neg(dr) then          ;;; divisor
        Bigint_do_negate(dr) -> dr;
        not(_negquot) -> _negquot
    else
        Bigint_copy(dr) -> dr
    endif;
    dr!BGI_LENGTH -> _drlen;

    ;;; unless divisor now shorter than dividend, quotient is 0
    if _drlen _greq _ddlen then
        ;;; remainder=dividend, quotient=0
        Clawback(0) -> ;            ;;; dummy to reclaim space used by copies
        return(Bigint_return(org_dd) ->, if _want_quot then 0 endif)
    endif;

    ;;; else it's full blown long division -- returns length of remainder
    ;;; and quotient if needed
    Long_divide(dd, dr, _negrem, _want_quot) -> _drlen;     ;;; rem length

    if _want_quot then
        -> quot;
        if _negquot then
            quot@BGI_SLICES -> _addr;
            Bigint_negate_range(_addr, quot!BGI_LENGTH, _addr);
        endif;
        ;;; quotient was last allocated
        Bigint_return(quot) -> Get_store() -> quot
    else
        0 -> quot
    endif;

    if dd == _nextfree_save then
        ;;; can reclaim end of remainder
        _drlen -> dd!BGI_LENGTH;
        Bigint_return(dd) -> _nextfree_save -> rm;  ;;; strip remainder or return integer
        Clawback(quot) -> quot
    else
        ;;; gc happened since dividend was copied, or copy not in saved seg
        ;;; strip remainder or return integer
        Bigint_return(Bigint_copy_len(dd, _drlen)) -> Get_store() -> rm
    endif;
    rm, if _want_quot then quot endif
enddefine;

global constant procedure (
    Bigint_rem  = Bgi_qrem(%false%),
    Bigint_//   = Bgi_qrem(%true%),
    );

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 20 1990
        Removed erroneously-placed (and unwanted)  memseg.ph
--- John Gibson, Dec 18 1989
        Changes for new pop pointers
 */
