/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/key.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *KEYS
 */

;;; ------------------------ KEY KEY ------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'gctypes.ph'

global constant
        procedure (Sys$-Bytevec_hashint)
    ;


;;; ---------------------------------------------------------------------

section $-Sys => class_access, iskey, key_key;

define Check_key(_item);
    lvars _item;
    unless iskey(_item) then
        mishap(_item, 1, 'KEY NEEDED')
    endunless
enddefine;

define Check_vkey(_item);
    lvars _item;
    unless iskey(_item) and _item!K_FLAGS _bitst _:M_K_VECTOR then
        mishap(_item, 1, 'VECTOR-CLASS KEY NEEDED')
    endunless
enddefine;

define Check_rkey(_item);
    lvars _item;
    unless iskey(_item) and _item!K_FLAGS _bitst _:M_K_RECORD then
        mishap(_item, 1, 'RECORD-CLASS KEY NEEDED')
    endunless
enddefine;

define Check_rorvkey(_item);
    lvars _item;
    unless iskey(_item)
    and _item!K_FLAGS _bitst (_:M_K_RECORD _biset _:M_K_VECTOR) then
        mishap(_item, 1, 'RECORD OR VECTOR -CLASS KEY NEEDED')
    endunless
enddefine;

define class_access(field_num, key);
    lvars key, field_num;
    Check_rkey(key);
    key!K_ACCESS_R -> key;
    Check_vsubscr(field_num, key);
    fast_subscrv(field_num, key)
enddefine;

define iskey(_item);
    lvars _item;
    iscompound(_item) and _item!KEY == key_key
enddefine;

define lconstant Key_print() with_nargs 1;
    Default_print(dup(), ()!K_DATAWORD)
enddefine;

define Key_hash() with_nargs 1;
    _pint(Bytevec_hashint(()!K_DATAWORD!W_STRING))
enddefine;

define lconstant Key_getsize(_key);
    lvars _key, _flags = _key!K_FLAGS, _gctype;
    if _flags _bitst _:M_K_VECTOR then
        ;;; vectorclass
        @@(struct KEY_V)++
    elseif _flags _bitst (_:M_K_RECORD _biset _:M_K_SPECIAL_RECORD) then
        ;;; recordclass/special record
        _key!K_GC_TYPE -> _gctype;
        if _gctype == _:GCTYPE_NFULLREC or _gctype == _:GCTYPE_USERNFREC then
            ;;; has full field offset table
            @@(w){@@K_FULL_OFFS_TAB{_key!K_FULL_OFFS_SIZE} | int.r}
                    _sub @@POPBASE
        else
            @@(struct KEY_R)++
        endif
    elseif _flags _bitst _:M_K_PROP_ENTRY then
        @@(struct KEY_PROP_ENTRY)++
    elseif _key!K_GC_TYPE == _:GCTYPE_USERNFVEC then
        @@(struct KEY_V)++
    else
        @@(struct KEY)++
    endif
enddefine;

constant
    key_key = struct KEY =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL _biset _:M_K_NONWRITEABLE,
                                ;;; K_FLAGS
        _:GCTYPE_KEY,           ;;; K_GC_TYPE
        Key_getsize,            ;;; K_GET_SIZE

        "key",                  ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        iskey,                  ;;; K_RECOGNISER
        WREF class_access,      ;;; K_APPLY
        nonop ==,               ;;; K_SYS_EQUALS
        WREF nonop ==,          ;;; K_EQUALS
        Key_print,              ;;; K_SYS_PRINT
        WREF Key_print,         ;;; K_PRINT
        WREF Key_hash,          ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE
        %};

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, May  5 1990
        Some improvements
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Apr  7 1988
        Moved out of primkeys.p, keys.p
 */
