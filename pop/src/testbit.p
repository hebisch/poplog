/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/testbit.p
 > Purpose:
 > Author:          John Gibson, Feb  1 1988 (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;; ---------------------- TEST A BIT -------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

;;; -----------------------------------------------------------------------

section $-Sys => testbit;

define testbit(x, _bitpos);
    lvars x, _bitpos, _sln;
    Check_integral(x);
    if _bitpos == 0 then
        ;;; optimise test on bit 0
        if issimple(x) then
            _int(x) -> x
        else
            x!BGI_SLICES[_0] -> x
        endif
    else
        Check_integer(_bitpos, 0);
        _int(_bitpos) -> _bitpos;
        if issimple(x) then
            _int(x) -> x;
            if _bitpos _gr _:POPINT_BITS then
                _:POPINT_BITS -> _bitpos        ;;; test sign bit
            endif
        else
            _bitpos _div _:SLICE_BITS -> _sln -> _bitpos;
            if _sln _greq x!BGI_LENGTH then
                x!BGI_LENGTH _sub _1 -> _sln;
                _:SLICE_BITS _sub _1 -> _bitpos     ;;; test sign bit
            endif;
            x!BGI_SLICES[_sln] -> x
        endif;
        _negate(_bitpos) -> _bitpos;
        _shift(x, _bitpos) -> x
    endif;
    x _bitst _1
enddefine;

define updaterof testbit(setit, x, _bitpos);
    lvars x, setit, _slice, _sln, _new, _bitpos, _op, _work = _NULL,
          _n_len;
    Check_integral(x);
    Check_integer(_bitpos, 0);
    _int(_bitpos) -> _bitpos;
    if setit then nonop _biset else nonop _biclear endif -> _op;
    if issimple(x) then
        _int(x) -> _slice;
        if _bitpos _lt _:POPINT_BITS then
            _shift(_1, _bitpos) -> _bitpos;
            return(_pint(_op(_slice, _bitpos)))
        elseif _op(_slice, _shift(_1, _:POPINT_BITS)) == _slice then
            return(x)
        elseunless testdef biginteger_key then
            mishap(x, _pint(_bitpos), 2, 'INTEGER OVERFLOW ON BIT UPDATE (bigintegers not loaded)')
        else
            BGWEAK work_bigint1 -> _work;
            SAVEWORKBGI(_work, _save1, _save2);
            BGWEAK Pint_to_bigint(x, _work) -> x
        endif
    endif;

    ;;; x now a biginteger
    _bitpos _div _:SLICE_BITS -> _sln -> _bitpos;
    if _sln _greq x!BGI_LENGTH then
        if BGWEAK Bigint_neg(x) then _-1 -> _slice else _0 -> _slice endif;
        _sln _add _1 -> _n_len;
    else
        x!BGI_SLICES[_sln] -> _slice;
        x!BGI_LENGTH -> _n_len;
    endif;
    if _sln _greq x!BGI_LENGTH _sub _1 and
       _bitpos _eq _:SLICE_BITS _sub _1 then
        _n_len _add _1 -> _n_len
    endif;
    _shift(_1, _bitpos) -> _bitpos;
    if (_op(_slice, _bitpos) ->> _new) == _slice then return(x) endif;
    BGWEAK Bigint_copy_len(x, _n_len) -> x;
    if _work /== _NULL then RESTWORKBGI(_work, _save1, _save2) endif;
    _new -> x!BGI_SLICES[_sln];     ;;; assign new slice
    BGWEAK Bigint_return(x) -> Get_store()
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 19 1995
        Used new macros SAVEWORKBGI/RESTWORKBGI to localise use of
        work_bigint1.
 */
