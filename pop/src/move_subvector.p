/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/move_subvector.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *DATA
 */

;;; ------------ MOVING/SETTING A SUBRANGE OF A VECTOR --------------------

#_INCLUDE 'declare.ph'

constant
        procedure (Sys$-Move_bits),
        _bitfield
    ;


;;; --------------------------------------------------------------------

section $-Sys => move_subvector, set_subvector;

define lconstant Check_vsubrange(_subs, vec, _len);
    lvars vec, _subs, _len;
    unless iscompound(vec) and vec!KEY!K_FLAGS _bitst _:M_K_VECTOR then
        mishap(vec, 1, 'VECTOR-TYPE STRUCTURE NEEDED')
    elseunless isinteger(_subs) and _subs fi_> 0 then
        mishap(_subs, 1, 'INTEGER > 0 NEEDED');
    elseunless _int(_subs) _add _len _sub _1 _lteq vec!V_LENGTH then
        mishap(_subs, vec, _pint(_len), 3,
                                'SUBVECTOR EXCEEDS VECTOR LENGTH')
    endunless
enddefine;

    /*  Move components of one vector to another
    */
define move_subvector(_sub1, vec1, _sub2, vec2, _len);
    lvars   vec1, vec2, procedure (p1, p2),
            _sub1, _sub2, _key1, _key2, _len, _fcode;

    Check_integer(_len, 0);
    _int(_len) -> _len;
    Check_vsubrange(_sub1, vec1, _len);
    Check_vsubrange(_sub2, vec2, _len);
    vec1!KEY -> _key1;
    vec2!KEY -> _key2;
    _key1!K_FIELD_CODE_V -> _fcode;
    if _key2!K_FIELD_CODE_V == _fcode then
        if _key1!K_SPEC == _key2!K_SPEC then
            ;;; same field code and spec -- use move subroutines
            _int(_sub1) _sub _1 -> _sub1;
            _int(_sub2) _sub _1 -> _sub2;
            go_on _pint(_fcode) to
            ;;;   1       2       3      4
                l_BYTE l_SHORT  l_INT l_DOUBLE
            else l_BIT;     ;;; i.e. if negative bit size

            l_BYTE:
                _bmove(@@(b)[_len], vec1@V_BYTES[_sub1], vec2@V_BYTES[_sub2]) ->;
                if _key1 == weakref dstring_key then
                    ;;; copy the attributes
                    _bmove(@@(b)[_len], _DSTRING_ATTR_PTR(vec1, _sub1),
                                            _DSTRING_ATTR_PTR(vec2, _sub2)) ->
                endif;
                return;
            l_SHORT:
                _smove(@@(s)[_len], vec1@V_SHORTS[_sub1], vec2@V_SHORTS[_sub2]) ->;
                if _key1 == weakref dstring16_key then
                    ;;; copy the attributes
                    _bmove(@@(b)[_len], _DSTRING16_ATTR_PTR(vec1, _sub1),
                                            _DSTRING16_ATTR_PTR(vec2, _sub2)) ->
                endif;
                return;
            l_INT:
                _imove(@@(i)[_len], vec1@V_INTS[_sub1], vec2@V_INTS[_sub2]) -> ;
                return;
            l_DOUBLE:
                _dmove(@@(d)[_len], vec1@V_DOUBLES[_sub1], vec2@V_DOUBLES[_sub2]) ->;
                return;
            l_BIT:
                Move_bits(_sub1, vec1, _sub2, vec2, _len, _negate(_fcode));
                return;

        elseif _key1!K_FLAGS _bitst _:M_K_STRING
        and _key2!K_FLAGS _bitst _:M_K_STRING then
            ;;; string(16) into dstring(16) or vice-versa
            _int(_sub1) _sub _1 -> _sub1;
            _int(_sub2) _sub _1 -> _sub2;
            if _key1!K_FLAGS _bitst _:M_K_STRING16 then
                _smove(@@(s)[_len], vec1@V_SHORTS[_sub1], vec2@V_SHORTS[_sub2]) -> ;
                if _key1 == string16_key then
                    ;;; vec1 a string16, vec2 a dstring16 -- zero the attributes
                    _bfill(_0, @@(b)[_len], _DSTRING16_ATTR_PTR(vec2, _sub2))
                ;;; else vec1 a dstring16, vec2 a string16
                endif
            else
                _bmove(@@(b)[_len], vec1@V_BYTES[_sub1], vec2@V_BYTES[_sub2]) -> ;
                if _key1 == string_key then
                    ;;; vec1 a string, vec2 a dstring -- zero the attributes
                    _bfill(_0, @@(b)[_len], _DSTRING_ATTR_PTR(vec2, _sub2))
                ;;; else vec1 a dstring, vec2 a string
                endif
            endif;
            return
        endif
    endif;

    ;;; default -- use fast_subscr procedures
    _key1!K_FAST_SUBSCR_V -> p1;
    _key2!K_FAST_SUBSCR_V!PD_UPDATER -> p2;
    until _zero(_len) do
        p1(_sub1, vec1);        ;;; get it
        p2(_sub2, vec2);        ;;; then put it
        _sub1 fi_+ 1 -> _sub1;
        _sub2 fi_+ 1 -> _sub2;
        _len _sub _1 -> _len
    enduntil
enddefine;

    /*  Set components of a vector to a given value
    */
define set_subvector(item, _subs, vec, _len);
    lvars vec, key, item, _ptr, _lim, _v1, _v2, _subs, _len, _bsize;

    Check_integer(_len, 0);
    _int(_len) -> _len;
    Check_vsubrange(_subs, vec, _len);
    returnif(_zero(_len));

    vec!KEY -> key;
    ;;; check item valid by assigning it into the first component
    fast_apply(item, _subs, vec, key!K_FAST_SUBSCR_V!PD_UPDATER);

    ;;; then replicate the field value in the other positions
    _int(_subs) _sub _1 -> _subs;

    go_on _pint(key!K_FIELD_CODE_V) to
    ;;;   1       2       3      4
        l_BYTE l_SHORT  l_INT l_DOUBLE
    else l_BIT;     ;;; i.e. if negative bit size

    l_BYTE:
        vec@V_BYTES[_subs] -> _ptr;
        _bfill(_ptr!(b), @@(b)[_len], _ptr);
        if key == weakref dstring_key then
            ;;; same for the attributes
            _DSTRING_ATTR_PTR(vec, _subs) -> _ptr;
            _bfill(_ptr!(b), @@(b)[_len], _ptr)
        endif;
        return;

    l_SHORT:
        vec@V_SHORTS[_subs] -> _ptr;
        _ptr@(s)[_len] -> _lim;
        _ptr!(s)++ -> _ptr -> _v1;
        while _ptr <@(s) _lim do
            _v1 -> _ptr!(s)++ -> _ptr
        endwhile;
        if key == weakref dstring16_key then
            ;;; same for the attributes
            _DSTRING16_ATTR_PTR(vec, _subs) -> _ptr;
            _bfill(_ptr!(b), @@(b)[_len], _ptr)
        endif;
        return;

    l_INT:
        vec@V_INTS[_subs] -> _ptr;
        _ifill(_ptr!(i), @@(i)[_len], _ptr);
        return;

    l_DOUBLE:
        vec@V_DOUBLES[_subs] -> _ptr;
#_IF ##(w)[_1|d] = _1
        _fill(_ptr!(w), @@(w)[_len], _ptr);
#_ELSE
        _ptr@(d)[_len] -> _lim;
        _ptr!(w)++ -> _ptr -> _v1;
        _ptr!(w)++ -> _ptr -> _v2;
        while _ptr <@(d) _lim do
            _v1 -> _ptr!(w)++ -> _ptr;
            _v2 -> _ptr!(w)++ -> _ptr
        endwhile;
#_ENDIF
        return;

    l_BIT:
        _negate(key!K_FIELD_CODE_V) -> _bsize;  ;;; bitsize of field
        unless _bsize == _1 then
            _subs _mult _bsize -> _subs;
            _len _mult _bsize -> _len
        endunless;
        @@V_BITS{_subs} -> _subs;
        _subs _add _len -> _lim;
        _bitfield(vec, _subs, _bsize) -> _v1;
        while (_subs _add _bsize ->> _subs) _lt _lim do
            _v1 -> _bitfield(vec, _subs, _bsize)
        endwhile;
        return;
enddefine;

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 11 1997
        String16 changes
--- John Gibson, Apr  6 1995
        Changed to allow word = double
--- John Gibson, Jan  4 1992
        Added -set_subvector-
--- John Gibson, Jan  3 1992
        Modified to deal with dstrings
--- John Gibson, Aug  2 1988
        Now uses Move_bits (movebits.p) to move bitfields.
--- John Gibson, Mar 21 1988
        Moved out of vectors.p
 */
