/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/integer_field.p
 > Purpose:
 > Author:          John Gibson, Feb  1 1988 (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;; -------------------- INTEGER FIELDS --------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

global constant
        4 ||/&
    ;

;;; ---------------------------------------------------------------------

section $-Sys => integer_field;

define lconstant Int_field_access(x, _bitsize, _bitpos, _mask, sign_mask);
    lvars   x, doshift = true, sign_mask, _bitpos, _mask, _bitsize,
            _work = _NULL;
    unless x then
        ;;; want unshifted field
        x -> doshift -> x
    endunless;
    Check_integral(x);
    _int(_bitpos) -> _bitpos;
    if issimple(_mask) then
        if issimple(x) then
            _int(_mask) -> _mask;
            _int(x) _bimask _mask -> x;
            if doshift then
                _negate(_bitpos) -> _bitpos;
                _shift(x, _bitpos) -> x;
                if sign_mask then
                    _int(sign_mask) -> sign_mask;
                    if x _bitst sign_mask then x _biset sign_mask -> x endif
                endif
            endif;
            return(_pint(x))
        endif;
        BGWEAK work_bigint1 -> _work;
        SAVEWORKBGI(_work, _save1, _save2);
        BGWEAK Pint_to_bigint(_mask, _work) -> _mask
    endif;

    ;;; biginteger(s) involved
    _CLAWBACK_SAVE;
    if issimple(x) then
        BGWEAK work_bigint1 -> _work;
        SAVEWORKBGI(_work, _save1, _save2);
        BGWEAK Pint_to_bigint(x, _work) -> x
    endif;
    BGWEAK Bigint_logical(nonop _bimask, x, _mask) -> x;

    if _work /== _NULL then RESTWORKBGI(_work, _save1, _save2) endif;

    if doshift then
        if issimple(x) then
            _negate(_bitpos) -> _bitpos;
            _pint(_shift(_int(x), _bitpos)) -> x
        else
            BGWEAK Bigint_>>(x, _bitpos) -> x
        endif;
        if sign_mask and x && sign_mask /== 0 then
            x || sign_mask -> x
        endif
    endif;
    Clawback(x)
enddefine;

define updaterof Int_field_access(new, x, _bitsize, _bitpos, _mask, _sign_mask);
    lvars x, new, doshift = true, mask, _bitpos, _mask, _bitsize, _sign_mask;
    _CLAWBACK_SAVE;
    unless x then
        ;;; new field is unshifted
        x -> doshift, new -> x -> new
    endunless;
    Check_integral(x);
    if doshift then
        new << _bitpos -> new
    else
        Check_integral(new)
    endif;
    if issimple(_mask) and issimple(new) and issimple(x) then
        _int(_mask) -> _mask;
        _int(x) -> x;
        return(_pint(_int(new) _bixor x _bimask _mask _bixor x))
    endif;

    _mask -> mask;
    Clawback(new ||/& x && mask ||/& x)
enddefine;

define integer_field(_bitsize, _bitpos) -> pdr;
    lvars pdr, mask, sign_mask = false, _bsize = _bitsize, _bitpos, _bitsize;
    Check_integer(_bitpos, 0);
    Check_integer(_bitsize, false);
    if _bsize == 0 then
        mishap(_bsize, 1, 'INTEGER /= 0 NEEDED')
    endif;
    if _bsize fi_< 0 then
        ;;; signed field
        - _bsize -> _bsize;
        -1 << (_bsize fi_- 1) -> sign_mask
    endif;
    (1 << _bsize - 1) << _bitpos -> mask;
    Consclos_protect(Int_field_access, _bitsize, _bitpos, mask, sign_mask, 4)
                                                                    -> pdr;
    pdr!PD_FLAGS _biset _:M_PD_CLOS_INTEGER_FIELD -> pdr!PD_FLAGS
enddefine;

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 19 1995
        Used new macros SAVEWORKBGI/RESTWORKBGI to localise use of
        work_bigint1.
 */
