/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/class_data.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *KEYS
 */

;;; ------------------- STRUCTURE KEYS ---------------------------------------

#_INCLUDE 'declare.ph'

global constant
        procedure sys_=
    ;

section $-Sys;

global constant
        procedure (Check_key, Check_rkey, Check_vkey, Check_rorvkey)
    ;

endsection;

define datasize(item);
    lvars item;
    if iscompound(item) then
        _pint( ##(w){fast_apply(item, item!KEY!K_GET_SIZE)} )
    else
        0
    endif
enddefine;

define class_dataword(key);
    lvars key;
    Sys$-Check_key(key);
    key!K_DATAWORD
enddefine;

define class_field_spec(key);
    lvars key;
    Sys$-Check_key(key);
    key!K_SPEC
enddefine;

define class_attribute(key);
    lvars key, word, _flags;
    if isword(key) then
        ;;; requesting info about specific attribute
        ((), key) -> (key, word);
        unless iskey(key) then Sys$-Check_key(key) endunless;
        key!K_FLAGS -> _flags;
        if     word == "external_ptr_props" then
            _flags _bitst _:M_K_EXTERN_PTR_PROPS
        elseif word == "external_ptr" then
            _flags _bitst _:M_K_EXTERN_PTR
        elseif word == "external_noconv" then
            ;;; is a struct passed directly without any conversion
            key!K_EXTERN_TYPE == _:EXTERN_TYPE_NORMAL
        elseif word == "external_deref" then
            key!K_EXTERN_TYPE == _:EXTERN_TYPE_DEREF
        elseif word == "byte_access" then
            _flags _bitst _:M_K_BYTE_ACCESS
        elseif word == "writeable" then
            if _flags _bitst _:M_K_WRITEABLE then
                true
            elseif _flags _bitst _:M_K_NONWRITEABLE then
                false
            else
                "undef"
            endif
        elseif word == "prop_entry" then
            if _flags _bitst _:M_K_PROP_ENTRY then
                key!K_PROP_TYPE_NAME
            else
                false
            endif
        else
            mishap(key, word, 2, 'class_attribute: UNKNOWN ATTRIBUTE NAME')
        endif

    else
        ;;; return list of attributes as given to conskey
        Sys$-Check_key(key);
        key!K_FLAGS -> _flags;
        [%  if key!K_FLAGS _bitst _:M_K_WRITEABLE then
                "writeable"
            elseif key!K_FLAGS _bitst _:M_K_NONWRITEABLE then
                "nonwriteable"
            endif;
            if _flags _bitst _:M_K_EXTERN_PTR then
                "external_ptr"
            elseif key!K_EXTERN_TYPE == _:EXTERN_TYPE_DEREF then
                "external_deref"
            endif
        %]
    endif
enddefine;

define class_datasize(key);
    lvars key;
    Sys$-Check_rkey(key);
    _pint( ##(w){key!K_RECSIZE_R} )
enddefine;

define class_recognise(key);
    lvars key;
    Sys$-Check_key(key);
    key!K_RECOGNISER
enddefine;

define class_print(key) -> p;
    lvars key, p;
    Sys$-Check_key(key);
    fast_cont(key!K_PRINT) -> p;
    if key!K_SYS_PRINT == p then
        sys_syspr -> p
    endif
enddefine;
;;;
define updaterof class_print(p, key);
    lvars key, p;
    Sys$-Check_key(key);
    if p == sys_syspr or p == syspr then
        key!K_SYS_PRINT -> p
    else
        Sys$-Check_procedure(p)
    endif;
    p -> fast_cont(key!K_PRINT)
enddefine;

define class_=(key) /* -> p */;
    lvars key;
    Sys$-Check_key(key);
    fast_cont(key!K_EQUALS)
enddefine;
;;;
define updaterof class_=(p, key);
    lvars key, p;
    Sys$-Check_key(key);
    if p == sys_= then
        key!K_SYS_EQUALS -> p
    else
        Sys$-Check_procedure(p)
    endif;
    p -> fast_cont(key!K_EQUALS)
enddefine;

define class_apply(key);
    lvars key;
    Sys$-Check_key(key);
    fast_cont(key!K_APPLY)
enddefine;
;;;
define updaterof class_apply(p, key);
    lvars key, p;
    Sys$-Check_key(key);
    Sys$-Check_procedure(p);
    p -> fast_cont(key!K_APPLY)
enddefine;

define class_hash(key);
    lvars key;
    Sys$-Check_key(key);
    fast_cont(key!K_HASH)
enddefine;
;;;
define updaterof class_hash(p, key);
    lvars p, key;
    Sys$-Check_key(key);
    Sys$-Check_procedure(p);
    if p == syshash then
        mishap(key, 1, 'CANNOT ASSIGN SYSHASH TO CLASS_HASH')
    endif;
    p -> fast_cont(key!K_HASH)
enddefine;

define class_cons(key);
    lvars key;
    Sys$-Check_rorvkey(key);
    if key!K_FLAGS _bitst _:M_K_RECORD then
        key!K_CONS_R
    else
        key!K_CONS_V
    endif
enddefine;

define class_dest(key);
    lvars key;
    Sys$-Check_rorvkey(key);
    if key!K_FLAGS _bitst _:M_K_RECORD then
        key!K_DEST_R
    else
        key!K_DEST_V
    endif
enddefine;

define class_init(key);
    lvars key;
    Sys$-Check_vkey(key);
    key!K_INIT_V;
enddefine;

define class_subscr(key);
    lvars key;
    Sys$-Check_vkey(key);
    key!K_SUBSCR_V;
enddefine;

define class_fast_subscr(key);
    lvars key;
    Sys$-Check_vkey(key);
    key!K_FAST_SUBSCR_V;
enddefine;

define isvectorclass(_item);
    lvars _item;
    if iscompound(_item)
    and (_item!KEY ->> _item)!K_FLAGS _bitst _:M_K_VECTOR then
        _item
    else
        false
    endif
enddefine;

define isrecordclass(_item);
    lvars _item;
    if iscompound(_item)
    and (_item!KEY ->> _item)!K_FLAGS _bitst _:M_K_RECORD then
        _item
    else
        false
    endif
enddefine;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 27 1996
        Added "byte_access" attribute to class_attribute
--- John Gibson, Dec  8 1995
        Stopped class_= returning sys_= and made it return the actual
        equals procedure.
--- John Gibson, Sep  4 1992
        Added "external_noconv" option to class_attribute
--- John Gibson, May 28 1992
        Allowed -class_attribute- to take just a key as arg, meaning return
        a list of -conskey- attributes.
--- John Gibson, Jan 15 1991
        Split -c*lass_spec- into -class_field_spec- and -class_atrtibute-.
--- John Gibson, May  5 1990
        More extensions to -c*lass_spec-
--- John Gibson, Mar 20 1990
        Removed is*externalclass
--- John Gibson, Mar 14 1990
        Changed -isexternalclass-
--- John Gibson, Jun 13 1989
        More extensions to -c*lass_spec- to enable key flags to be tested
--- John Gibson, May 25 1989
        Added extensions to -c*lass_spec-
--- Roger Evans, Jun  8 1988
        Added -isexternalclass-
--- John Gibson, Apr  7 1988
        Renamed class_data.p (previously keys.p). Some stuff moved to
        key.p
--- John Gibson, Feb 21 1988
        Replaced -Check_subscr- with -Sys$-Check_vsubscr-
--- John Gibson, Sep  4 1987
        Added use of K_SYS_PRINT procedure in new key format
 */
