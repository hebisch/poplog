/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/vec_generic.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; ---------------- GENERIC VECTOR PROCEDURES ---------------------------

#_INCLUDE 'declare.ph'

global constant
        procedure (Sys$-Key_hash, Sys$-Get_store_dalign)
    ;

;;; --------------------------------------------------------------------

section $-Sys => sysanyvecons;


;;; --- HASHING PROCEDURES --------------------------------------------

    /*  Hash a sequence of _len chars. These procedures are used (amongst other
        things) for getting the bucket of a string in the dictionary.
        If the size of the dictionary or the hashing algorithm is changed,
        the corresponding procedure word_dict_cell in src/syscomp/w_util.p
        must be changed also (since poplink sets up the initial dictionary)
    */
define :inline lconstant CHAR_HASH(T);
    if _len _sgr _2 then
        ;;; use first, middle and last chars and length
        _cptr!(T)[_0] _add _len
        _add _shift(_cptr!(T)[_len _sub _1] _add _len, _3)
        _add _shift(_cptr!(T)[_shift(_len,_-1)] _add _len, _6) -> _len;
        _shift(_len, _-10) _add _len
    elseif _len == _2 then
        ;;; use first and last chars
        _cptr!(T)[_0] _add _shift(_cptr!(T)[_1], _3)
    elseif _len == _1 then
        ;;; just use first character
        _cptr!(T)[_0]
    else
        _0
    endif
enddefine;

define Byte_hash(_cptr, _len);
#_IF false
    lvars _len, _cptr;
    CHAR_HASH(b)
#_ELSE
    lvars _res = _len;
    while _len _sgr _3 do
        _cptr!(i) _add _res -> _res;
        _len _add _-4 -> _len;
        _cptr _add _4 -> _cptr;
    endwhile;
    if _len == _3 then
        (_cptr!(i) _bimask _16:FFFFFF) _add _res -> _res;
    elseif _len == _2 then
        _cptr!(s) _add _res -> _res;
    elseif _len == _1 then
        _cptr!(b)[_0] _add _res -> _res;
    endif;
   ;;; Mix bits
   _shift(_res _bimask _16:FFFE0000, _-17) _bixor _res
#_ENDIF
enddefine;

define Short_hash(_cptr, _len);
    lvars _len, _cptr;
    CHAR_HASH(s)
enddefine;

define Bytevec_hashint(_bvec);
    lvars _bvec;
    if _bvec!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        chain(_bvec@V_SHORTS, _bvec!V_LENGTH, Short_hash)
    else
        chain(_bvec@V_BYTES, _bvec!V_LENGTH, Byte_hash)
    endif
enddefine;

define Hash_vector_between(item, _lo, _hi);
    lvars item, key = item!KEY, _lo, _hi;
    if key!K_FIELD_CODE_V == _:t_BYTE then
        _int(_lo) _sub _1 -> _lo;
        _pint(Byte_hash(item@V_BYTES[_lo], _int(_hi) _sub _lo))
    elseif _lo fi_> _hi then
        Key_hash(key)
    else
        Key_hash(key)
            fi_+ (_hi fi_- _lo)
            fi_+ syshash(fast_apply(_hi, item, key!K_FAST_SUBSCR_V))
    endif
enddefine;

define Vector_hash(item) with_props '(Sys$-Vector_hash)';
    lvars item;
    Hash_vector_between(item, 1, _pint(item!V_LENGTH))
enddefine;


;;; --- APPLYING A VECTOR --------------------------------------------------

define Vector_apply(vec) with_props '(Sys$-Vector_apply)';
    lvars vec, _vsub;
    unless _nonzero(_stklength()) and (->> _vsub; isinteger(_vsub)) then
        ;;; assume should be executing non-procedure
        Exec_nonpd(vec)
    elseif _int(_vsub) _sub _1 _greq vec!V_LENGTH then
        ;;; subscript out of range - make this give the error
        fast_apply(vec, vec!KEY!K_SUBSCR_V)
    else
        fast_apply(vec, vec!KEY!K_FAST_SUBSCR_V)
    endunless
enddefine;
;;;
define updaterof Vector_apply(vec) with_props '(->Sys$-Vector_apply)';
    lvars vec, _vsub;
    unless _nonzero(_stklength()) and (->> _vsub; isinteger(_vsub)) then
        ;;; assume should be executing non-procedure
        -> Exec_nonpd(vec)
    elseif _int(_vsub) _sub _1 _greq vec!V_LENGTH then
        ;;; subscript out of range - make this give the error
        fast_apply(vec, vec!KEY!K_SUBSCR_V!PD_UPDATER)
    else
        fast_apply(vec, vec!KEY!K_FAST_SUBSCR_V!PD_UPDATER)
    endunless
enddefine;


;;; --- CHECK PROCEDURES ------------------------------------------------

define Check_bytevec(_item);
    lvars _item, _flags;
    unless iscompound(_item)
    and ((_item!KEY!K_FLAGS ->> _flags) _bitst _:M_K_STRING
         or (_flags _bitst _:M_K_VECTOR
            and _item!KEY!K_FIELD_CODE_V == _:t_BYTE))
    then
        mishap(_item, 1, 'STRING OR BYTE VECTOR NEEDED')
    endunless
enddefine;

define Check_vsubscr(_subs, _item);
    lvars _item, _subs;
    unless isinteger(_subs) and _int(_subs) _sub _1 _lt _item!V_LENGTH then
        mishap(_subs, _item, 2, 'BAD SUBSCRIPT FOR INDEXED ACCESS')
    endunless
enddefine;


;;; --- GETSIZE PROCEDURES ----------------------------------------------

define Bytevec_getsize() with_nargs 1;      ;;; byte vector
    _BYTEVEC_SIZE(()!V_LENGTH)
enddefine;

define Wordvec_getsize() with_nargs 1;      ;;; word/full vector
    @@V_WORDS[()!V_LENGTH] _sub @@POPBASE
enddefine;


;;; --- EQUALS PROCEDURES ----------------------------------------------

define Eq__Fullvec(item, fvec) with_props '(Sys$-Eq__Fullvec)';
    lvars item, fvec, _work, _lim;
    if item == fvec then
        true
    elseif issimple(item) then
        false
    elseif (item!KEY ->> _work) == fvec!KEY and item!V_LENGTH == fvec!V_LENGTH
    then
        _checkall();            ;;; check for interrupt/recursion overflow
        @@V_WORDS[_0] -> _work;
        @@V_WORDS[fvec!V_LENGTH] -> _lim;
        repeat
            returnif(_work _greq _lim) (true);
            returnunless(EQ( item!(w){_work}, fvec!(w){_work} )) (false);
            @@(w){_work}++ -> _work;
            _CHECKINTERRUPT
        endrepeat
    elseif _work!K_FLAGS _bitst _:M_K_MATCH_VAR then
        fast_chain(fvec, item, _work!K_SYS_EQUALS)
    else
        false
    endif
enddefine;


;;; --- CONSTRUCTING VECTORS -----------------------------------------------

    /*  Get a raw (i.e. uninitialised) vector
    */
define Get_rawvec(_len, key, _zinit) -> _vec;
    lvars key, _vec, _len, _offs, _zinit;
    lconstant _dummy_vec = (writeable inits(_pint( ##(b)[_1|struct POPREC1] )
                                                ))@V_WORDS@~POPBASE;
    ;;; create a dummy _vec with length and key
    ;;; so we can call the getsize procedure
    _len -> _dummy_vec!V_LENGTH;
    key -> _dummy_vec!KEY;
    fast_apply(_dummy_vec, key!K_GET_SIZE) -> _offs;
    if key!K_FLAGS _bitst _:M_K_DOUBLE_ALIGN then
        ;;; vector must be doubleword aligned
        Get_store_dalign(_offs)
    else
        Get_store(_offs)
    endif -> _vec;
    if _zinit then
        ;;; initialise to zero
        _fill(_0, _offs, _vec@POPBASE)
    else
        ;;; just make sure padding is zero (last word)
        _0 -> _vec@POPBASE[_-1]!(w){_offs}
    endif;
    key -> _vec!KEY;
    _len -> _vec!V_LENGTH
enddefine;


define sysanyvecons(stacklen) with_nargs 2;
    lvars stacklen, procedure cons_p;
    lconstant errms = 'STACK UNDERFLOW IN VECTOR CONSTRUCTOR';
    if isinteger(stacklen) then
        ;;; cons procedure is the first vector 'element'
        if (stacklength() fi_- stacklen fi_- 1 ->> stacklen) fi_< 0 then
            mishap(0, errms)
        endif;
        _user_sp()!(w)[_int(stacklen)] -> cons_p;
        Check_procedure(cons_p);
        cons_p(stacklen) -> stacklen;
        () -> ;                 ;;; erase the procedure
        stacklen
    else
        ;;; cons procedure is the last arg
        ((), stacklen) -> (stacklen, cons_p);
        if (stacklength() fi_- stacklen ->> stacklen) fi_< 0 then
            mishap(0, errms)
        endif;
        Check_procedure(cons_p);
        cons_p(stacklen)
    endif
enddefine;

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 12 1997
        String16 changes
--- John Gibson, Feb 17 1996
        Added pdprops string to Eq__Fullvec
--- John Gibson, Jan  4 1996
        Changed Eq__Fullvec to use EQ macro
--- John Gibson, Jun  8 1995
        sysanyvecons now also allows the constructor procedure to appear
        above the vector items on the stack
--- John Gibson, Apr 13 1992
        Fixed -Hash_vector_between- to use new algorithm as well
--- John Gibson, Apr 10 1992
        Improved hashing algorithm in -Bytevec_hashint-
--- John Gibson, Sep 14 1991
        Changes for new format bytevecs.
--- John Gibson, Mar 24 1990
        Changed tests for byte vector in -Hash_vector_between- and
        -Check_bytevec- to use K_FIELD_CODE_V.
--- John Gibson, Dec  2 1989
        Changes for new pop pointers.
--- John Gibson, Aug  5 1988
        Changed -Get_rawvec- to allocate double-aligned vectors when
        needed.
--- John Gibson, Apr 13 1988
        Most of old vectors.p
--- John Gibson, Apr 13 1988
        Full vectors to vector.p
--- John Gibson, Apr  5 1988
        Moved -issubstring- and -issubstring_lim- to issubstring.p.
        Added -isstartstring-.
--- John Gibson, Mar 28 1988
        Moved intvec, shortvec and fsub_ stuff to separate files
--- John Gibson, Mar 21 1988
        -move_subvector- moved to separate file
--- John Gibson, Mar 14 1988
        -stringin- moved to separate file
--- John Gibson, Mar  7 1988
        Moved -sysvecons- and -sysanyvecons- into here from pop11_syntax.p
--- John Gibson, Feb 28 1988
        Moved in -Wordvec_getsize-, -Shortvec_getsize-
--- John Gibson, Feb 22 1988
        Various procedures into section Sys
--- John Gibson, Feb 21 1988
        Moved basic string stuff into new file string.p
--- John Gibson, Feb 11 1988
        Uint_->_pint, Check_integer etc in section Sys
 */
