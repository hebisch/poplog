/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/bigint_random.p
 > Purpose:
 > Author:          John Gibson, Feb  2 1988
 */

;;; ----------------- BIGINTEGER RANDOM GENERATION ---------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

constant
        procedure (Sys$-Random_genseed)
    ;

;;; ---------------------------------------------------------------------

section $-Sys;

    ;;; random (big)integer from bigint n > 0
define Bigint_random(n);
    lvars n, ranbig, _ptr, _slice, _lim, _len, _seed = Random_genseed();
    _CLAWBACK_SAVE;
    ;;; get random bigint 1 slice longer than n
    n!BGI_LENGTH _add _1 -> _len;   ;;; 1 slice longer than n
    Get_bigint(_len) -> ranbig;
    ranbig@BGI_SLICES[_0] -> _ptr;
    _ptr@(SL)[_len] -> _lim;
    while _ptr <@(SL) _lim do
#_IF valof("BIGINT_SPEC") == "short"
        ;;; shift down top SLICE_BITS for short slices
        _shift(_seed, _:SLICE_BITS _sub _:RANSEED_BITS) -> _seed;
#_ELSE
        ;;; use two seeds for each slice
        _shift(_seed, #_< _int(-RANSEED_BITS>>1) >_#) -> _seed;
        Random_genseed() _bixor _seed -> _seed;
#_ENDIF
        _seed -> _ptr!(SL)++ -> _ptr;
        Random_genseed() -> _seed       ;;; next seed
    endwhile;

    ;;; result is then (ranbig * n) >> _len slices
    fast_apply(ranbig, n, fast_subscrv(OP_*, Bigint_ops)) -> n;
    if iscompound(n) and n!BGI_LENGTH _gr _len then
        n@BGI_SLICES[_0] -> _slice;
        _slice@(SL)[n!BGI_LENGTH] -> _lim;
        _slice@(SL)[_len] -> _ptr;
        while _ptr <@(SL) _lim do
            _ptr!(SL)++ -> _ptr -> _slice!(SL)++ -> _slice
        endwhile;
        n!BGI_LENGTH _sub _len -> n!BGI_LENGTH;
        Bigint_return(n) -> Get_store() -> n
    else
        0 -> n
    endif;
    Clawback(n)
enddefine;


endsection;     /* $-Sys */
