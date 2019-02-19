/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/integer_length.p
 > Purpose:
 > Author:          John Gibson, Mar 14 1988 (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;; ------------------ BIT LENGTH OF AN INTEGER ---------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

;;; -------------------------------------------------------------------------

section $-Sys => integer_length;

define integer_length(x);
    lvars x, _len;

    define lconstant Int_length(x);
        lvars x, y, _n;
        if _neg(x) then _-1 _sub x -> x endif;
        returnif(_zero(x)) (_0);
        _1 -> _n;
    #_< lvars b = _pint(##(1)[_1|w]);
        while b >= 4 do
            b >> 1 -> b;
            [if _nonzero(_shift(x, %_int(-b)%) ->> y) then
                _n _add %_int(b)% -> _n,  y -> x
            endif;].dl;
        endwhile;
    >_#
        repeat
            returnif(_zero(_shift(x, _-1) ->> x)) (_n);
            _n _add _1 -> _n
        endrepeat
    enddefine;

    if isinteger(x) then
        Int_length(_int(x))
    elseif iscompound(x) and x!KEY == weakref biginteger_key then
        x!BGI_LENGTH _sub _1 -> _len;
        Int_length(x!BGI_SLICES[_len]) _add (_len _mult _:SLICE_BITS)
    else
        ;;; mishap
        Check_integral(x)
    endif;
    _pint()
enddefine;

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 20 1995
        Revised Int_length to work for any wordsize
 */
