/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/bigint_logical.p
 > Purpose:
 > Author:          John Gibson, Feb  2 1988
 */


;;; -------------- BIGINTEGER LOGICAL OPERATIONS ------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'


section $-Sys;

    ;;; logical operation (e.g. _biset, etc) on two bigintegers
define Bigint_logical(_op, x, y);
    lvars x, y, result,
        _Raddr, _Xaddr, _Yaddr, _Rlim, _Xlim, _Ylim, _op
        ;

    ;;; sign extension of a bigint, given the address of
    ;;; after its last component
    define lconstant Bgi_sign_extn() with_nargs 1;
        if _neg(!(-SL)[_-1]) then _:SLICE_MASK else _0 endif;
    enddefine;

    x!BGI_LENGTH ->> _Rlim -> _Xlim;
    y!BGI_LENGTH -> _Ylim;
    ;;; get greater of the two lengths in _Rlim
    if _Ylim _gr _Xlim then _Ylim -> _Rlim endif;
    Get_bigint(_Rlim) -> result;

    ;;; set up addresses
    result@BGI_SLICES -> _Raddr;
    _Raddr@(SL)[_Rlim] -> _Rlim;
    x@BGI_SLICES -> _Xaddr;
    _Xaddr@(SL)[_Xlim] -> _Xlim;
    y@BGI_SLICES -> _Yaddr;
    _Yaddr@(SL)[_Ylim] -> _Ylim;

    ;;; get sign extensions of x and y
    Bgi_sign_extn(_Xlim) -> x;
    Bgi_sign_extn(_Ylim) -> y;

    repeat
        _op(
            if _Xaddr <@(SL) _Xlim then
                (_Xaddr!(SL)++ -> _Xaddr)
            else
                x
            endif,
            if _Yaddr <@(SL) _Ylim then
                (_Yaddr!(SL)++ -> _Yaddr)
            else
                y
            endif,
            ) -> _Raddr!(SL)++ -> _Raddr;
        quitunless(_Raddr <@(SL) _Rlim)
    endrepeat;

    Bigint_return(result) -> Get_store()
enddefine;

endsection;     /* $-Sys */
