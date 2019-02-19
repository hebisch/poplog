/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/fi_logical.p
 > Purpose:
 > Author:          John Gibson, Feb  2 1988
 > Documentation:   REF *FASTPROCS
 */

;;;------------------- FAST LOGICAL PROCEDURES --------------------------------

#_INCLUDE 'declare.ph'


lconstant macro
    _POPINT_BITS    = _int(POPINT_BITS);

define 4 fi_|| with_nargs 2;    ;;; logical OR
    lvars x;
    _int() -> x;
    _pint(_int() _biset x)
enddefine;

define 4 fi_&& with_nargs 2;    ;;; logical AND
    lvars x;
    _int() -> x;
    _pint(_int() _bimask x)
enddefine;

define 4 fi_&&~~ with_nargs 2;  ;;; logical AND complement (bit clear)
    lvars x;
    _int() -> x;
    _pint(_int() _biclear x)
enddefine;

define 4 fi_||/& with_nargs 2;  ;;; logical exclusive OR
    lvars x;
    _int() -> x;
    _pint(_int() _bixor x)
enddefine;

define 4 fi_~~ with_nargs 1;    ;;; logical NOT
    _pint(_logcom(_int()))
enddefine;

define 4 fi_>> n with_nargs 2;  ;;; shift right
    lvars n;
    if _neg(_negate(_int(n)) ->> n) then
        if n _slt _negate(_POPINT_BITS) then    ;;; right shift
            _negate(_POPINT_BITS) -> n
        endif
    elseif n _sgr _POPINT_BITS then             ;;; left shift
        ->, return(0)
    endif;
    _pint(_shift(_int(), n))
enddefine;

define 4 fi_<< n with_nargs 2;  ;;; shift left
    lvars n;
    if _neg(_int(n) ->> n) then
        if n _slt _negate(_POPINT_BITS) then    ;;; right shift
            _negate(_POPINT_BITS) -> n
        endif
    elseif n _sgr _POPINT_BITS then             ;;; left shift
        ->, return(0)
    endif;
    _pint(_shift(_int(), n))
enddefine;
