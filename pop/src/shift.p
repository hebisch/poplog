/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/shift.p
 > Purpose:
 > Author:          John Gibson, Feb  2 1988
 > Documentation:   REF *NUMBERS
 */

;;;----------------- LOGICAL SHIFT LEFT/RIGHT --------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

global constant
        _pshift_testovf
    ;

;;; ------------------------------------------------------------------------

section $-Sys =>  <<  >> ;

lconstant macro
    _POPINT_BITS    = _int(POPINT_BITS);

define lconstant Shiftval_error();
    mishap((), 1, 'INTEGER SHIFT AMOUNT NEEDED')
enddefine;

    ;;; shift right, (sysint) _n positive
define lconstant Logical_>>(x, _n);
    lvars x, _n;
    if isinteger(x) then
        if _n _sgr _POPINT_BITS then _POPINT_BITS -> _n endif;
        _negate(_n) -> _n;
        _pint(_shift(_int(x), _n))
    elseif iscompound(x) and x!KEY == weakref biginteger_key then
        BGWEAK Bigint_>>(x, _n)
    else
        ;;; mishap
        Check_integral(x)
    endif
enddefine;

    ;;; shift left, (sysint) _n positive
define lconstant Logical_<<(x, _n);
    lvars x, _n;
    if isinteger(x) then
        ;;; overflow to bigint possible
        unless _pshift_testovf(x, _n) then
            ;;; overflows
            if testdef biginteger_key then
                BGWEAK Bigint_op_int_bgint(x, _n, BGWEAK Bigint_<<)
            else
                mishap(x, _pint(_n), 2, 'INTEGER OVERFLOW (bigintegers not loaded)')
            endif
        ;;; else result is on stack
        endunless
    elseif iscompound(x) and x!KEY == weakref biginteger_key then
        BGWEAK Bigint_<<(x, _n)
    else
        ;;; mishap
        Check_integral(x)
    endif
enddefine;

define 4 >> n with_nargs 2;     ;;; shift right
    lvars n;
    unless isinteger(n) then Shiftval_error(n) endunless;
    if _neg(_int(n) ->> n) then
        Logical_<<(_negate(n))
    else
        Logical_>>(n)
    endif
enddefine;

define 4 << n with_nargs 2;     ;;; shift left
    lvars n;
    unless isinteger(n) then Shiftval_error(n) endunless;
    if _neg(_int(n) ->> n) then
        Logical_>>(_negate(n))
    else
        Logical_<<(n)
    endif
enddefine;

endsection;     /* $-Sys */
