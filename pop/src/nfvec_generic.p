/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/nfvec_generic.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; ----------------- NON=FULL VECTOR PROCEDURES ------------------------

#_INCLUDE 'declare.ph'

;;; --------------------------------------------------------------------

section $-Sys;

define Eq__Nfullvec(item, nfvec) with_props '(Sys$-Eq__Nfullvec)';
    lvars item, nfvec, _key;
    if item == nfvec then
        true
    elseif issimple(item) then
        false
    elseif (item!KEY ->> _key) == nfvec!KEY and item!V_LENGTH == nfvec!V_LENGTH
    then
        ;;; can compare by words because any padding is
        ;;; guaranteed to be zero
        _cmp(fast_apply(item, _key!K_GET_SIZE) _sub (@@V_WORDS _sub @@POPBASE),
                                            item@V_WORDS, nfvec@V_WORDS)
    elseif _key!K_FLAGS _bitst _:M_K_MATCH_VAR then
        fast_chain(nfvec, item, _key!K_SYS_EQUALS)
    else
        false
    endif
enddefine;

define Bitvec_getsize(bvec) with_nargs 1;   ;;; bit vector
    lvars bvec, _key;
    bvec!KEY -> _key;
    if issimple(_key) then _mkcompound(_key) -> _key endif;
    @@V_WORDS{ _negate(_key!K_FIELD_CODE_V) _mult bvec!V_LENGTH | 1.r}
                                                    _sub @@POPBASE
enddefine;

define Shortvec_getsize() with_nargs 1;     ;;; short vector
    @@V_WORDS[()!V_LENGTH | short.r] _sub @@POPBASE
enddefine;

define Intvec_getsize() with_nargs 1;       ;;; int vector
    @@V_WORDS[()!V_LENGTH | int.r] _sub @@POPBASE
enddefine;

define Doublevec_getsize() with_nargs 1;    ;;; double vector
    @@V_WORDS[()!V_LENGTH | d] _sub @@POPBASE
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 17 1996
        Added pdprops string to Eq__Nfullvec
--- John Gibson, Dec  2 1989
        Changes for new pop pointers
--- John Gibson, Mar 15 1989
        Moved getsize procedures in from conskey.p
--- John Gibson, Apr 13 1988
        Moved out of vectors.p
 */
