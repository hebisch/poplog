/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/integer_bit.p
 > Purpose:
 > Author:          John Gibson, Feb  1 1988 (see revisions)
 > Documentation:   REF *NUMBERS
 */


;;; ------------------ INTEGER BIT PROCEDURES --------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

;;; -------------------------------------------------------------------------

section $-Sys => integer_leastbit, integer_bitcount;

define integer_leastbit(x);
    lvars x, _n = _0;
    if isinteger(x) then
        returnif(_zero(_int(x) ->> x)) (false);
        until x _bitst _2:11111111 do
            _n _add _8 -> _n; _shift(x, _-8) -> x
        enduntil;
        unless x _bitst _2:1111 then
            _n _add _4 -> _n; _shift(x, _-4) -> x
        endunless;
        unless x _bitst _2:11 then
            _n _add _2 -> _n; _shift(x, _-2) -> x
        endunless;
        unless x _bitst _2:1 then
            _n _add _1 -> _n
        endunless;
        _pint(_n)
    elseif iscompound(x) and x!KEY == weakref biginteger_key then
        _pint(BGWEAK Bigint_leastbit(x))
    else
        ;;; mishap
        Check_integral(x)
    endif
enddefine;

define integer_bitcount(x);
    lvars x, _ptr, _count, _sgnslice, _last;

    define lconstant Int_bitcount(x);
        lvars x, _n;
        if _neg(x) then _logcom(x) -> x endif;
        if _zero(x) then return(_0) endif;
        _1 -> _n;
        until _zero((x _sub _1) _bimask x ->> x) do
            _n _add _1 -> _n
        enduntil;
        _n
    enddefine;

    if isinteger(x) then
        Int_bitcount(_int(x)) -> _count
    elseif iscompound(x) and x!KEY == weakref biginteger_key then
        x@BGI_SLICES[_0] -> _ptr;
        _ptr@(SL)[x!BGI_LENGTH _sub _1] -> _last;
        _last!(-SL) -> _sgnslice;
        Int_bitcount(_sgnslice) -> _count;
        while _ptr <@(SL) _last do
            Int_bitcount(_ptr!(SL)++ -> _ptr),
                if _neg(_sgnslice) then _negate() _add _:SLICE_BITS endif
                            _add _count -> _count
        endwhile;
    else
        ;;; mishap
        Check_integral(x)
    endif;
    _pint(_count)
enddefine;

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 20 1995
        Improved integer_leastbit
--- John Gibson, Mar 14 1988
        Moved -integer_length- to separate file
 */
