/* --- Copyright University of Sussex 1987. All rights reserved. ----------
 > File:            C.all/src/logical.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;----------------------- LOGICAL PROCEDURES --------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

constant
        Sys$-bigint_minus_1
    ;

;;; ------------------------------------------------------------------------

section $-Sys =>  ||  &&  &&~~  ||/&  ~~ ;

define lconstant Ints_needed();
    mishap((), (), 2, '(BIG)INTEGER(S) NEEDED')
enddefine;

    ;;; bigint logical operation, where x is a simple integer, y is not
define lconstant Logical_2A(x, y, _op);
    lvars x, y, _op;
    if iscompound(y) and y!KEY == weakref biginteger_key then
        BGWEAK Bigint_op_int_bgint(_op, x, y, BGWEAK Bigint_logical)
    else
        Ints_needed(x, y)
    endif
enddefine;

    ;;; bigint logical operation, where x not an integer, y unknown
define lconstant Logical_2B(x, y, _op);
    lvars x, y, _op;
    if iscompound(x) and x!KEY == weakref biginteger_key then
        if isinteger(y) then
            return(BGWEAK Bigint_op_bgint_int(_op, x, y, BGWEAK Bigint_logical))
        elseif iscompound(y) and y!KEY == weakref biginteger_key then
            return(BGWEAK Bigint_logical(_op, x, y))
        endif
    endif;
    Ints_needed(x, y)
enddefine;

define 4 x || y;                ;;; logical OR
    lvars x, y;
    if isinteger(x) then
        if isinteger(y) then
            _int(y) -> y;
            _pint(_int(x) _biset y)
        else
            Logical_2A(x, y, nonop _biset)
        endif
    else
        Logical_2B(x, y, nonop _biset)
    endif
enddefine;

define 4 x && y;                ;;; logical AND
    lvars x, y;
    if isinteger(x) then
        if isinteger(y) then
            _int(y) -> y;
            _pint(_int(x) _bimask y)
        else
            Logical_2A(x, y, nonop _bimask)
        endif
    else
        Logical_2B(x, y, nonop _bimask)
    endif
enddefine;

define 4 x &&~~ y;              ;;; logical AND complement (bit clear)
    lvars x, y;
    if isinteger(x) then
        if isinteger(y) then
            _int(y) -> y;
            _pint(_int(x) _biclear y)
        else
            Logical_2A(x, y, nonop _biclear)
        endif
    else
        Logical_2B(x, y, nonop _biclear)
    endif
enddefine;

define 4 x ||/& y;              ;;; logical exclusive OR
    lvars x, y;
    if isinteger(x) then
        if isinteger(y) then
            _int(y) -> y;
            _pint(_int(x) _bixor y)
        else
            Logical_2A(x, y, nonop _bixor)
        endif
    else
        Logical_2B(x, y, nonop _bixor)
    endif
enddefine;

define 4 ~~ x;                  ;;; logical NOT
    lvars x;
    if isinteger(x) then
        _pint(_logcom(_int(x)))
    elseif iscompound(x) and x!KEY == weakref biginteger_key then
        BGWEAK Bigint_logical(nonop _biclear, BGWEAK bigint_minus_1, x)
    else
        ;;; mishap
        Check_integral(x)
    endif
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb  9 1988
        Changes for splitting up files, sectioning, putting in weakrefs, etc.
--- John Gibson, Jan 12 1988
        Corrected bug in &&/=_0 (didn't mishap if first arg was a
        structure other than a biginteger).
--- John Gibson, Dec  6 1987
        Removed unnecessary procedure name after "nonexported".
--- John Gibson, Sep 15 1987
        Replaced signed bigint slice type sSL with -SL
 */
