/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/int_convert.p
 > Purpose:
 > Author:          John Gibson, Feb  2 1988 (see revisions)
 */

;;; ------------ POP <-> SYSTEM INTEGER CONVERSION ------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

;;; -----------------------------------------------------------------------

section $-Sys;

    ;;; values used for converting between sysints and bigints, etc
lconstant
    ICVT_SHIFT          = WORD_BITS-POPINT_BITS,
    _ICVT_SHIFT         = _int(ICVT_SHIFT),
    _ICVT_LOMASK        = _int(2**ICVT_SHIFT-1),
    _ICVT_HIMASK        = _int(2**POPINT_BITS-1),
    ;

define lconstant No_bgi();
    mishap(0, 'OVERFLOW CONVERTING TO POP INTEGER (bigintegers not loaded)')
enddefine;


    ;;;  unsigned int -> pop (big)integer
define Uint_->_pint(_n);
    lvars res, _n, _work;
    returnif(_nonneg(_n) and _pint_testovf(_n)) (/* result on stack */);
    unless testdef biginteger_key then No_bgi() endunless;
    ;;; else split in two, produce bigint from masked hi part, add low part
    BGWEAK work_bigint1 -> _work;
    SAVEWORKBGI(_work, _save1, _save2);
    BGWEAK Bigint_<< (
        BGWEAK Pint_to_bigint(
            _pint( _shift(_n, _negate(_ICVT_SHIFT)) 
                                  _bimask _ICVT_HIMASK),
            _work),
        _ICVT_SHIFT) -> res;
    RESTWORKBGI(_work, _save1, _save2);
    _n _bimask _ICVT_LOMASK -> _n;
    res!BGI_SLICES[_0] _add _n -> res!BGI_SLICES[_0];   ;;; + low bits
    res
enddefine;

    ;;; signed int -> pop (big)integer
define Sint_->_pint(_n);
    lvars res, _n, _work;
    returnif(_pint_testovf(_n)) (/* result on stack */);
    unless testdef biginteger_key then No_bgi() endunless;
    ;;; else split in two, produce bigint from hi part, add low part
    BGWEAK work_bigint1 -> _work;
    SAVEWORKBGI(_work, _save1, _save2);
    BGWEAK Bigint_<< (
        BGWEAK Pint_to_bigint( _pint(_shift(_n, _negate(_ICVT_SHIFT))),
                                                _work),
        _ICVT_SHIFT) -> res;
    RESTWORKBGI(_work, _save1, _save2);
    _n _bimask _ICVT_LOMASK -> _n;
    res!BGI_SLICES[_0] _add _n -> res!BGI_SLICES[_0];   ;;; + low bits
    res
enddefine;

define lconstant Err(_rangemask, _signed);
    lvars _rangemask, _signed;
    mishap((), 1, 'INTEGER ' sys_>< if _signed then
                                        Sint_->_pint(_logcom(_rangemask))
                                    else
                                        0
                                    endif
                            sys_>< ' TO '
                            sys_>< Uint_->_pint(_rangemask)
                            sys_>< ' NEEDED')
enddefine;

    ;;; pop (big)integer -> unsigned int
define Pint_->_uint(n, _rangemask);
    lvars n, _hi, _lo, _rangemask;
    if isinteger(n) then
                _int(n) -> _hi;
        returnif(_nonneg(_hi) and _hi _lteq _rangemask) (_hi);

    elseif iscompound(n) and n!KEY == weakref biginteger_key then
        n!BGI_SLICES[_0] _bimask _ICVT_LOMASK -> _lo;
        BGWEAK Bigint_>>(n, _ICVT_SHIFT) -> _hi;
        if isinteger(_hi) then
            _int(_hi) -> _hi;
            if _hi _lteq _ICVT_HIMASK then
                _shift(_hi, _ICVT_SHIFT) _add _lo -> _hi;
                returnif(_hi _lteq _rangemask) (_hi)
            endif
        endif;
    endif;
    Err(n, _rangemask, false)
enddefine;

    ;;; pop (big)integer -> signed int
define Pint_->_sint(n, _rangemask);
    lvars n, _hi, _lo, _mask, _rangemask;
        if(isinteger(n)) then
           _int(n) -> _hi;
           if _logcom(_rangemask) _slteq _hi and _hi _slteq _rangemask then
              return (_hi);
           endif
    elseif iscompound(n) and n!KEY == weakref biginteger_key then
        n!BGI_SLICES[_0] _bimask _ICVT_LOMASK -> _lo;
        BGWEAK Bigint_>>(n, _ICVT_SHIFT) -> _hi;
        if isinteger(_hi) then
            _int(_hi) -> _hi;
            _shift(_rangemask, _negate(_ICVT_SHIFT)) -> _mask;
            if _logcom(_mask) _slteq _hi and _hi _slteq _mask then
                return(_shift(_hi, _ICVT_SHIFT) _add _lo)
            endif
        endif
    endif;
    Err(n, _rangemask, true)
enddefine;

    ;;; simple pop integer -> unsigned int
define Simpint_->_uint(n, _rangemask);
    lvars n, _rangemask;
    if isinteger(n) then
        _int(n) -> n;
        returnif(n _lteq _rangemask) (n);
        _pint(n) -> n
    endif;
    Err(n, _rangemask, false)
enddefine;

    ;;; simple pop integer -> signed int
define Simpint_->_sint(n, _rangemask);
    lvars n, _rangemask;
    if isinteger(n) then
        _int(n) -> n;
        returnif(_logcom(_rangemask) _slteq n and n _slteq _rangemask) (n);
        _pint(n) -> n
    endif;
    Err(n, _rangemask, true)
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 19 1995
        Used new macros SAVEWORKBGI/RESTWORKBGI to localise use of
        work_bigint1.
--- John Gibson, Mar 14 1989
        Moved simple integer checking procedures in from fields.p
 */
