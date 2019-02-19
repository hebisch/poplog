/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/bigint_gcd.p
 > Purpose:
 > Author:          John Gibson, Jan 22 1988
 */

;;; -------------- GREATEST COMMON DIVISOR OF 2 BIGINTEGERS ----------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

constant
        procedure (Sys$-Intgr_gcd, Sys$-Bigint_rem)
    ;


;;; ---------------------------------------------------------------------

section $-Sys;

define Bigint_gcd(u, v);
    lvars u, v, _k, _l;

    ;;; Find the gcd of two bigintegers, both odd.
    ;;; Uses the 'differencing' algorithm from Knuth, but modified
    ;;; to employ division whenever the two numbers are of different
    ;;; magnitudes (i.e. bigints of different lengths)
    define lconstant Bgi_gcd(u, v);
        lvars u, v, t, procedure (bgi_+ = fast_subscrv(OP_+, Bigint_ops),
            bgi_negate = fast_subscrv(OP_negate, Bigint_ops) ),
            _vneg
            ;
        ;;; get a bigint and then discard it to make sure there's enough space
        ;;; to do the whole thing (including divisions) without a gc happening
        ;;; or without changing segments
        Get_bigint(_shift(u!BGI_LENGTH _add v!BGI_LENGTH _add _8, _2))
                                                            -> Get_store();
        _CLAWBACK_SAVE;

        ;;; copy/negate both to get u > 0, v < 0
        Bigint_neg(v) -> _vneg;
        if Bigint_neg(u) then
            if _vneg then bgi_negate(v) else Bigint_copy(v) endif,
                Bigint_copy(u) -> v -> u
        else
            if _vneg then Bigint_copy(v) else bgi_negate(v) endif -> v;
            Bigint_copy(u) -> u
        endif;

        ;;; now u > 0, v < 0 and both odd
        repeat
            if issimple(u) or issimple(v) then
                chain(Clawback(u), Clawback(v), Intgr_gcd)
            endif;
            ;;; do addition if same length, division if different
            if u!BGI_LENGTH == v!BGI_LENGTH then
                ;;; diff is even
                bgi_+(u, v) -> t
            else
                if u!BGI_LENGTH _gr v!BGI_LENGTH then
                    if (Bigint_rem(u, v) ->> t) == 0 then v -> u endif
                else
                    Bigint_rem(v, u) -> t
                endif
            endif;
            if issimple(t) then
                if t == 0 then return(Clawback(u)) endif;
                if t fi_< 0 then t -> v else t -> u endif
            else
                (t, Bigint_leastbit(t));
                if Bigint_neg(t) then
                    Bigint_>>_into(v) -> v
                else
                    Bigint_>>_into(u) -> u
                endif;
                t -> Get_store()            ;;; reclaim t
            endif
        endrepeat
    enddefine;      /* Bgi_gcd */

    Bigint_>>(u, Bigint_leastbit(u) ->> _k) -> u;
    Bigint_>>(v, Bigint_leastbit(v) ->> _l) -> v;
    if _l _lt _k then _l -> _k endif;
    if issimple(u) or issimple(v) then
        Intgr_gcd(u, v)
    else
        Bgi_gcd(u, v)
    endif << _pint(_k)
enddefine;


endsection;     /* $-Sys */
