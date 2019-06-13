/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/misc_fields.p
 > Purpose:
 > Author:          John Gibson, Jun  3 1996
 */

;;; ------------ MISCELLANEOUS FIELD ACCESSING PROCEDURES -----------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

constant
        procedure (is_poplog_item, is_fixed),
    ;


;;; -------------------------------------------------------------------------

section $-Sys$-Fld;

lconstant macro (
    W1REM_BITS  = WORD_BITS - SLICE_BITS,
    W1REM_MASK  = 0,
    W2_BITS     = SLICE_BITS - W1REM_BITS,
    W2REM_BITS  = WORD_BITS - W2_BITS,
    W2REM_MASK = 0,
);

    /*  Check the value to go into an "exval" field -- identical translation
        to that performed by _call_external, except structures to be passed
        must be fixed address.
        (NB: referenced by Popc, so must be perm)
    */
define Exval_val(item) /* -> _item */;
    lvars item, _type;
    lstackmem sfloat _tmpflt;

    if issimple(item) then
        if isinteger(item) then
            _int(item)
        else
            FLWEAK _pf_sfloat_dec(item)
        endif
    elseif (item!KEY!K_EXTERN_TYPE ->> _type) == _:EXTERN_TYPE_NORMAL then
        if is_fixed(item) then
            item
        else
            mishap(item, 1, 'FIXED-ADDRESS STRUCTURE NEEDED')
        endif
    elseif _type == _:EXTERN_TYPE_DEREF then
        item!XP_PTR
    elseif _type == _:EXTERN_TYPE_DDEC then
        item -> FLWEAK Float_val_s(_tmpflt);
        _tmpflt!(sfloat)
    else
        ;;; biginteger -- return modulo wordsize
        lvars _val = item!BGI_SLICES[_0];
        if item!BGI_LENGTH /== _1 then
             _val _bimask _:SLICE_MASK -> _val;
            _shift(item!BGI_SLICES[_1], _:SLICE_BITS) _add _val -> _val;
           ;;; FIXME: looks bogus
#_IF WORD_BITS==DOUBLE_BITS
            if item!BGI_LENGTH /== _2 then
                _shift(item!BGI_SLICES[_2], _:SLICE_BITS _add _:SLICE_BITS)
                                    _add _val -> _val
            endif
#_ENDIF
        endif;
        _val
    endif
enddefine;

    /*  Check the value coming out of an external pointer "full" field
        (NB: referenced by Popc, so must be perm)
    */
define Full_val_extern(/* value */) with_nargs 1;
    unless is_poplog_item(dup()) or dup() == _NULL then
        mishap((), 1, 'EXTERNAL full FIELD CONTAINS JUNK VALUE')
    endunless
enddefine;


#_IF WORD_BITS /== DOUBLE_BITS

    /*  Code for accessing doubleword int fields (i.e. (u)longlong)
    */

define lconstant Bgi_err(val, signed);
    lvars val, signed, lo, hi;
    if isintegral(val) then
        if signed then
            -1 << #_<DOUBLE_BITS-1>_#,  1 << #_<DOUBLE_BITS-1>_# - 1
        else
            0, 1<<DOUBLE_BITS - 1
        endif -> (lo, hi);
        mishap(val, 1, 'INTEGER ' sys_>< lo sys_>< ' TO ' sys_>< hi
                                        sys_>< ' NEEDED')
    else
        mishap(val, 1, '(BIG)INTEGER NEEDED')
    endif
enddefine;

define :inline lconstant DOUB_VAL(_addr, lo_only_test, unsigned_mask_expr);
    lvars bint, _lo, _hi;
#_IF DEF BIG_ENDIAN \n
    _addr!(w)[_0] -> _hi;
    _addr!(w)[_1] -> _lo;
#_ELSE
    _addr!(w)[_0] -> _lo;
    _addr!(w)[_1] -> _hi;
#_ENDIF
    if lo_only_test then
        ;;; hi is just sign-extension of lo
        returnif(_pint_testovf(_lo)) (/* result on stack */);
    endif;
    unless testdef biginteger_key then
        mishap(0, 'OVERFLOW CONVERTING TO POP INTEGER (bigintegers not loaded)')
    endunless;
    BGWEAK Get_bigint(_3) -> bint;
    _lo -> bint!BGI_SLICES[_0];
    _shift(_lo, _:-SLICE_BITS) _bimask _:W1REM_MASK -> _lo;
    (_shift(_hi, _:W1REM_BITS) _add _lo)
                                    -> bint!BGI_SLICES[_1];
    _shift(_hi, _:-W2_BITS) unsigned_mask_expr -> bint!BGI_SLICES[_2];

    BGWEAK Bigint_return(bint) -> Get_store()
enddefine;

define :inline lconstant DOUB_UPD(val, _addr, signed, sign_extn,
                                                        signed_add_expr);
    lvars _lo, _hi;
    if isinteger(val) then
        _int(val) -> _lo;
        sign_extn -> _hi;
    elseif isbiginteger(val) then
        val!BGI_SLICES[_0] -> _lo;
        if val!BGI_LENGTH == _1 then
            sign_extn -> _hi
        else
            val!BGI_SLICES[_1] -> _hi;
            _shift(_hi, _:SLICE_BITS) _add _lo -> _lo;
            _shift(_hi, _:-W1REM_BITS) -> _hi;
            if val!BGI_LENGTH == _2 then
                if _neg(_hi) and not(signed) then Bgi_err(val, signed) endif
            else
                Bgi_err(val, signed)
            endif
        endif
    else
        Bgi_err(val, signed)
    endif;
#_IF DEF BIG_ENDIAN \n
    _hi -> _addr!(w)[_0];
    _lo -> _addr!(w)[_1];
#_ELSE
    _lo -> _addr!(w)[_0];
    _hi -> _addr!(w)[_1];
#_ENDIF
enddefine;

    /*  Access/update a signed double(word) field (pdprops is the value spec)
        (NB: referenced by Popc, so must be perm)
    */
define Double_val_s(_addr) with_props #_< -DOUBLE_BITS >_#;
    lvars _addr;
    DOUB_VAL(_addr, _shift(_lo, _:-WORD_BITS _add _1) == _hi, /*empty*/)
enddefine;
;;;
define updaterof Double_val_s(val, _addr);
    lvars val, _addr;
    DOUB_UPD(val, _addr, true, if _neg(_lo) then _-1 else _0 endif, 0);
enddefine;

    /*  Access/update an unsigned double(word) field (pdprops is the value spec)
        (NB: referenced by Popc, so must be perm)
    */
define Double_val_u(_addr) with_props DOUBLE_BITS;
    lvars _addr;
    DOUB_VAL(_addr, _zero(_hi) and _nonneg(_lo), _bimask _:W2REM_MASK)
enddefine;
;;;
define updaterof Double_val_u(val, _addr);
    lvars val, _addr;
    DOUB_UPD(val, _addr, false, if _neg(_lo) then Bgi_err(val, false)
                                else _0
                                endif, /*empty*/)
enddefine;

#_ENDIF


endsection;     /* $-Sys$-Fld */
