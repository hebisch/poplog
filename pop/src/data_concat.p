/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/data_concat.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *DATA
 */

;;; ----------- CONCATENATE TWO STRUCTURES OF THE SAME CLASS -------------

#_INCLUDE 'declare.ph'

section $-Sys;

constant
        procedure (<>_strings, Cons_pcomposite, Flush_procedure,
        Get_rawvec, Move_bits)
    ;

endsection;

;;; ----------------------------------------------------------------------

section $-Sys =>  <>, nc_<> ;

define lconstant <>_procedures(p1, p2) -> comp;
    lvars p1, p2, comp, _comp;

    ;;; get composite procedure without fields filled in -- keep in a non-pop
    ;;; variable until all fields safely assigned
    Cons_pcomposite() -> _comp;

    ;;; initialise header (PD_EXECUTE, PD_LENGTH etc already filled in)
    procedure_key -> _comp!KEY;
    false       ->> _comp!PD_PROPS -> _comp!PD_UPDATER;
    p1          -> _comp!PD_COMPOSITE_P1;
    p2          -> _comp!PD_COMPOSITE_P2;
    _:M_PD_COMPOSITE -> _comp!PD_FLAGS;
    _0          -> _comp!PD_FLAGS2;

    ;;; now safe to put in a pop var
    _comp -> comp;

#_IF DEF CACHEFLUSH
    Flush_procedure(comp);
#_ENDIF

    if p1!PD_FLAGS _bitst _:M_PD_CLOSURE then
        _int(pdnargs(p1))
    else
        p1!PD_NARGS
    endif -> comp!PD_NARGS;

    if p2!PD_UPDATER then
        ;;; make updater P1 <> updater(P2)
        copy(comp) ->> _comp -> comp!PD_UPDATER;
        p2!PD_UPDATER -> _comp!PD_COMPOSITE_P2
    endif
enddefine;

    /*  Concatenate 2 vectors of the same kind
    */
define lconstant <>_vectors(vec1, vec2) -> _vec;
    lvars   vec1, key, vec2, _vec,
            _len1 = vec1!V_LENGTH, _len2 = vec2!V_LENGTH;

    returnif(_zero(_len1)) (vec2 -> _vec);
    returnif(_zero(_len2)) (vec1 -> _vec);
    vec1!KEY -> key;

    Get_rawvec(_len1 _add _len2, key, false) -> _vec;

    ;;; move in words of first vector
    _moveq( fast_apply(vec1,key!K_GET_SIZE) _sub (@@V_WORDS _sub @@POPBASE),
                                    vec1@V_WORDS, _vec@V_WORDS) -> ;

    ;;; now move in fields of second
    go_on _pint(key!K_FIELD_CODE_V) to
    ;;; 1       2       3       4
        l_BYTE l_SHORT l_INT l_DOUBLE
    else l_BIT;     ;;; i.e. if negative bit size

    l_BYTE:
        _bmove(@@(b)[_len2], vec2@V_BYTES[_0], _vec@V_BYTES[_len1]) -> ;
        return;

    l_SHORT:
        _smove(@@(s)[_len2], vec2@V_SHORTS[_0], _vec@V_SHORTS[_len1]) -> ;
        return;

    l_INT:
        _imove(@@(i)[_len2], vec2@V_INTS[_0], _vec@V_INTS[_len1]) -> ;
        return;

    l_DOUBLE:
        _dmove(@@(d)[_len2], vec2@V_DOUBLES[_0], _vec@V_DOUBLES[_len1]) -> ;
        return;

    l_BIT:
        ;;; bit aligned
        Move_bits(_0, vec2, _len1, _vec, _len2, _negate(key!K_FIELD_CODE_V))
enddefine;      /* <>_vectors */

define lconstant <>_lists(list1, list2);
    lvars list1, last, newlist, list2;
    if null(list1) then
        list2
    elseif list2 == [] then
        ;;; (can't test null here because it would expand the list)
        list1
    else
        conspair(fast_front(list1), 0) ->> last -> newlist;
        fast_back(list1) -> list1;
        until null(list1) do
            conspair(fast_front(list1), 0) ->> fast_back(last) -> last;
            fast_back(list1) -> list1;
        enduntil;
        list2 -> fast_back(last);
        newlist
    endif
enddefine;      /* <>_lists */


define 5 item1 <> item2;
    lvars item1, item2, _key1 = datakey(item1), _key2 = datakey(item2);

    if (_key1 == pair_key or item1 == [])
    and (_key2 == pair_key or item2 == []) then
        return(<>_lists(item1, item2))

    elseif _key1!K_FLAGS _bitst _:M_K_STRING
    and _key2!K_FLAGS _bitst _:M_K_STRING then
        return(<>_strings(item1, item2))

    elseif _key1 == _key2 then
        if _key1!K_FLAGS _bitst _:M_K_VECTOR then
            return(<>_vectors(item1, item2))
        elseif _key1 == word_key then
            item1!W_STRING -> _key1;
            item2!W_STRING -> _key2;
            returnif(_zero(_key1!V_LENGTH)) (Cons_word(_key2, false));
            returnif(_zero(_key2!V_LENGTH)) (Cons_word(_key1, false));
            return(Cons_word(<>_strings(_key1, _key2), true))
        elseif _key1 == procedure_key then
            ;;; compose two procedures
            return(<>_procedures(item1, item2))
        endif
    endif;
    mishap(item1, item2, 2, 'ILLEGAL ARGUMENTS FOR <>')
enddefine;

    ;;; same, but without copying the first argument if this is possible,
    ;;; i.e. when concatenating lists
define -5 item1 nc_<> item2;
    lvars item1, next, item2;
    unless (ispair(item1) or item1 == []) and (ispair(item2) or item2 == [])
    then
        item1 <> item2
    elseif null(item1) then
        item2
    else
        item1;              ;;; the result
        repeat
            fast_back(item1) -> next;
            quitif(null(next));
            next -> item1
        endrepeat;
        item2 -> fast_back(item1)
    endunless
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 12 1997
        String16 changes
--- John Gibson, May 27 1995
        Initialised new PD_FLAGS2 field to 0 in <>_procedures.
--- John Gibson, Jan  3 1992
        Modified <> to work concatenating a string to a dstring etc
--- Robert John Duncan, Feb 11 1991
        Added cache flush
--- John Gibson, Feb 12 1989
        Common code from pdr_compose.p moved to <>_procedures, etc.
--- John Gibson, Aug  6 1988
        Fixed bug in concatenating words when one was a null word
        (been there for an awfully long time...)
--- John Gibson, Apr  9 1988
        Moved out of data.p
 */
