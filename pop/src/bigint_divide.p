/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/src/bigint_divide.p
 > Purpose:
 > Author:          John Gibson, Feb  2 1988 (see revisions)
 */

;;; ------------------ BIGINTEGER DIVISION --------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

global constant
        _emul, _ediv, _bgi_mult, _bgi_mult_add
    ;

section $-Sys;

constant
        procedure (Bigint_div_range, Bigint_div_single,
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


define lconstant Long_divide(dividend, divisor, _negrem, _want_quot);
    lvars quot, rm, dividend, divisor,
        _Dlim, _DRlim, _Daddr, _hi, _lo, _msdr, _DRstart, _DRoff, _Dstart,
        _Qlim, _nextd, _fact, _negrem, _want_quot
    ;

    define lconstant Add_back(_DRaddr, _DRlim, _Daddr) -> carry;
        lvars carry = _0, _DRaddr, _DRlim, _Daddr;
        repeat
            (_DRaddr!(SL)++ -> _DRaddr) _add _Daddr!(SL) _add carry
                            -> carry;
            if carry _bitst _:SIGN_BIT then
                carry _biclear _:SIGN_MASK -> _Daddr!(SL)++ -> _Daddr;
                _1 -> carry
            else
                carry -> _Daddr!(SL)++ -> _Daddr;
                _0 -> carry
            endif;
            quitunless(_DRaddr <@(SL) _DRlim)
        endrepeat
    enddefine;

    dividend!BGI_LENGTH -> _Dlim;
    divisor!BGI_LENGTH -> _DRlim;
    if _want_quot then
        Get_bigint(_Dlim _sub _DRlim) ->> quot; ;;; leave on stack as result
        quot@BGI_SLICES[quot!BGI_LENGTH] -> _Qlim
    endif;
    dividend@BGI_SLICES -> _Dstart;
    _Dstart@(SL)[_Dlim] -> _Dlim;
    divisor@BGI_SLICES -> _DRstart;
    @@(SL)[_DRlim] -> _DRoff;                   ;;; divisor length as offset
    _DRstart@(SL){_DRoff} -> _DRlim;

    ;;; get the factor with which to multiply both dividend and divisor,
    ;;; making the ms slice of the divisor as large as possible
    ;;; (this leaves the quotient unaffected, but the remainder is times _fact)
    _DRlim!(SL)[_-1] -> _msdr;              ;;; ms slice of divisor
    if _msdr _bitst _:MOST_POSITIVE_BIT then
        ;;; _fact is just 1, multiplies not necessary
        _1 -> _fact
    else
        _ediv(_1, _0, _msdr _add _1) -> _fact -> ;
        ;;; multiply dividend by it (ms slice of dividend is initially 0)
        _bgi_mult(_fact, _Dstart, _Dlim, _Dstart) -> -> ;
        ;;; multiply divisor by it
        _bgi_mult(_fact, _DRstart, _DRlim, _DRstart) -> -> ;
        _DRlim!(SL)[_-1] -> _msdr           ;;; new ms slice of divisor
    endif;

    repeat
        _Dlim--!(SL) -> _Dlim -> _nextd;    ;;; next dividend slice
        if _nextd == _msdr then
            _:MOST_POSITIVE -> quot;
            ;;; this can overflow to SLICE_BITS+1, but it doesn't matter
            ;;; because then _hi must be _lteq rm
            _Dlim!(SL)[_-1] _add _msdr -> rm
        else
            _ediv(_nextd, _Dlim!(SL)[_-1], _msdr) -> quot -> rm
        endif;
        repeat
            _emul(_DRlim!(SL)[_-2], quot) -> _hi -> _lo;
            quitif(_hi _lteq rm
                    or _hi == rm and _lo _lteq _Dlim!(SL)[_-2] );
            quot _sub _1 -> quot;
            _DRlim!(SL)[_-2] _add rm -> rm
        endrepeat;

        ;;; subtract quot times divisor from dividend
        _Dlim@(SL)-{_DRoff} -> _Daddr;  ;;; where divisor is aligned to dividend
        unless _zero(quot) then
            ;;; do mult by - _msdr and add last carry to top dividend slice
            (_bgi_mult_add(_negate(quot), _DRstart, _DRlim, _Daddr) ->)
                    _add _nextd -> _nextd;
            ;;; if result goes negative, add back divisor and reduce quot by 1
            if _neg(_nextd) then
                Add_back(_DRstart, _DRlim, _Daddr) _add _nextd -> _nextd;
                quot _sub _1 -> quot
            endif;
unless _zero(_nextd) then
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
    _Dlim@(SL)++ -> _Dlim;

    ;;; finally correct the remainder, dividing it by _fact
    if _fact == _1 then
        if _negrem then
            ;;; if truncated, allow extra slice for overflow
            if _Dlim /== _Daddr then _Dlim@(SL)++ -> _Dlim endif;
            Bigint_negate_range(_Dstart, _Dlim, _Dstart) -> ;;; can't overflow
        endif
    else
        if _negrem then _negate(_fact) -> _fact endif;
        Bigint_div_range(_fact, _Dstart, _Dlim, _Dstart) -> ;;; erase 0 remainder
    endif;

    ;;; return length of remainder
    ##(SL){_Dlim, _Dstart}
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
                            Bigint_return(Bigint_do_negate(dd)) -> Get_store()
                        endif)
        elseif _x == _1 then
            return(0, if _want_quot then Bigint_return(dd) -> endif)
        endif;
        Get_bigint(_ddlen) -> quot;
        chain(dd, _x, quot, _want_quot, Bigint_div_single)
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
        if Bigint_negate_range(_addr, _addr@(SL)[_ddlen _sub _1], dd@BGI_SLICES) then
            ;;; overflowed
            _ddlen _add _1 -> _ddlen
        else
            ;;; didn't overflow, can truncate 1
            _ddlen -> dd!BGI_LENGTH;
            dd@V_WORDS[_ddlen|SL.r]@~POPBASE -> Get_store()
        endif;
        _0 -> dd!BGI_SLICES[_ddlen _sub _1];    ;;; 0 most sig slice
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
            Bigint_negate_range(_addr, _addr@(SL)[quot!BGI_LENGTH], _addr) ->
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
