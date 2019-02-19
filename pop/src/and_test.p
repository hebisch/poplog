/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/and_test.p
 > Purpose:
 > Author:          John Gibson, Feb  2 1988 (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;; -------------------- LOGICAL AND TEST ---------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'


;;; -----------------------------------------------------------------------

section $-Sys =>  &&/=_0  &&=_0 ;

define 6 x &&/=_0 y;
    lvars x, y, _Xaddr, _Yaddr, _Xlim, _Ylim, _work = _NULL;
    if isinteger(x) then
        if isinteger(y) then
            _int(y) -> y;
            return(_int(x) _bitst y)
        elseif iscompound(y) and y!KEY == weakref biginteger_key then
            BGWEAK work_bigint1 -> _work;
            SAVEWORKBGI(_work, _save1, _save2);
            BGWEAK Pint_to_bigint(x, _work) -> x
        else
            goto ERR
        endif
    elseif iscompound(x) and x!KEY == weakref biginteger_key then
        if isinteger(y) then
            BGWEAK work_bigint1 -> _work;
            SAVEWORKBGI(_work, _save1, _save2);
            BGWEAK Pint_to_bigint(y, _work) -> y
        elseunless iscompound(y) and y!KEY == weakref biginteger_key then
            goto ERR
        endif
    else
        goto ERR
    endif;

    ;;; &&/=_0 for bigintegers
    ;;; make x the longer of the two
    if y!BGI_LENGTH _gr x!BGI_LENGTH then x, y, -> x -> y endif;
    x@BGI_SLICES[_0] -> _Xaddr;
    _Xaddr@(SL)[x!BGI_LENGTH] -> _Xlim;
    y@BGI_SLICES[_0] -> _Yaddr;
    _Yaddr@(SL)[y!BGI_LENGTH] -> _Ylim;
    repeat
        _Yaddr!(-SL)++ -> _Yaddr -> y;
        if (_Xaddr!(-SL)++ -> _Xaddr) _bitst y then true, goto RETURN endif;
        quitunless(_Yaddr <@(SL) _Ylim);
    endrepeat;
    if _neg(y) then
        while _Xaddr <@(SL) _Xlim do
            if _nonzero(_Xaddr!(-SL)++ -> _Xaddr) then true, goto RETURN endif;
        endwhile
    endif;
    false;
RETURN:
    if _work /== _NULL then RESTWORKBGI(_work, _save1, _save2) endif;
    return;

ERR:
    mishap(x, y, 2, '(BIG)INTEGER(S) NEEDED')
enddefine;

define 6 &&=_0 with_nargs 2;
    not(&&/=_0)
enddefine;


endsection;     /* $-Sys */

/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 19 1995
        Used new macros SAVEWORKBGI/RESTWORKBGI to localise use of
        work_bigint1.
 */
