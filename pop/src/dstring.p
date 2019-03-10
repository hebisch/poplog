/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/dstring.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *STRINGS
 */

;;; ------------- STRINGS WITH DISPLAY ATTRIBUTES --------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'gctypes.ph'

constant
        procedure (subdstring)
    ;

section $-Sys;

constant
        procedure (Str_hash, Str_print, Eq__String16, Eq__String8,
        Vector_apply, Fill_string, Dest_string, Check_bytevec_subrange,
        Get_string16, Get_string, Substring)
    ;

endsection;


;;; ------------------------------------------------------------------------


section $-Sys => isdstring, initdstring, initdstring16,
                 consdstring, destdstring,
                 subscrdstring, fast_subscrdstring, subdstring,
                 dstring_key, dstring16_key;

lconstant macro (
    INV8    = ~~16:FF00FF,
    INV16   = ~~16:FFFFFF,
);

define :inline lconstant _DSTRING_SIZE(_len);
    @@V_WORDS{_shift(_BYTEVEC_DATA_SIZE(_len), _1)} _sub @@POPBASE
enddefine;

define :inline lconstant _DSTRING16_SIZE(_len);
    @@V_WORDS{_STRING16_DATA_SIZE(_len) _add _BYTEVEC_DATA_SIZE(_len)}
                                _sub @@POPBASE
enddefine;

define isdstring(_item);
    lvars _item;
    iscompound(_item) and _item!KEY!K_FLAGS _bitst _:M_K_DSTRING
enddefine;

define Get_dstring(_len) -> _dstring;
    lvars _dstring, _len, _size = _DSTRING_SIZE(_len);
    Get_store(_size) -> _dstring;
    dstring_key -> _dstring!KEY;
    _len -> _dstring!V_LENGTH;
    ;;; ensure any padding bytes at the end of both parts are zero
    _0 -> _dstring@(w)[_-1]!(w){_BYTEVEC_LIM_OFFS(_len)};
    _0 -> _dstring@POPBASE[_-1]!(w){_size}
enddefine;
;;;
define updaterof Get_dstring(_dstring, _newlen, all8);
    lvars   nkey, all8, _ptr, _bptr, _lim, _alim, _dstring, _newlen, _astart,
            _new_astart;

    if _dstring!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        _dstring@(w){_STRING16_LIM_OFFS(_dstring!V_LENGTH)} -> _astart;
        if all8 then
            ;;; 16 -> 8
            _dstring@V_SHORTS[_0] -> _ptr;
            _ptr@(s)[_newlen] -> _lim;
            _dstring@V_BYTES[_0] -> _bptr;
            while _ptr <@(s) _lim do
                (_ptr!(s)++ -> _ptr) -> _bptr!(b)++ -> _bptr
            endwhile;
            dstring_key -> _dstring!KEY;
            goto DO_8
        endif;
        _dstring@(w){_STRING16_LIM_OFFS(_newlen)} -> _new_astart;
        ;;; pad string16 part correctly
        _new_astart@(w->s) -> _lim;
        _dstring@V_SHORTS[_newlen] -> _ptr;
        while _ptr <@(s) _lim do _0 -> _ptr!(s)++ -> _ptr endwhile;
        string16_key -> nkey
    else
        _dstring@(w){_BYTEVEC_LIM_OFFS(_dstring!V_LENGTH)} -> _astart;
DO_8:
        _dstring@(w){_BYTEVEC_LIM_OFFS(_newlen)} -> _new_astart;
        ;;; pad string part correctly
        _new_astart@(w->b) -> _lim;
        _dstring@V_BYTES[_newlen] -> _ptr;
        while _ptr <@(b) _lim do _0 -> _ptr!(b)++ -> _ptr endwhile;
        string_key -> nkey
    endif;
    _newlen -> _dstring!V_LENGTH;

    ;;; pad attribute part correctly
    _astart@(w){_BYTEVEC_DATA_SIZE(_newlen)} -> _alim;
    _alim@(w->b) -> _lim;
    _astart@(w->b)[_newlen] -> _ptr;
    while _ptr <@(b) _lim do _0 -> _ptr!(b)++ -> _ptr endwhile;

    ;;; see if all attributes are zero, and optimise to a string(16) if so
    _astart -> _ptr;
    while _ptr <@(w) _alim do
        if _nonzero(_ptr!(w)++ -> _ptr) then
            _ptr--@(w) -> _ptr;
            quitloop
        endif
    endwhile;

    if _ptr <@(w) _alim then
        ;;; must keep as dstring(16) -- move attributes down
        _moveq(@@(w){_alim, _astart}, _astart, _new_astart)
    else
        ;;; can make a string(16)
        nkey -> _dstring!KEY;
        _new_astart
    endif@~POPBASE -> Get_store()   ;;; truncate to correct size
enddefine;

define Get_dstring16(_len) -> _dstring;
    lvars _dstring, _len, _size = _DSTRING16_SIZE(_len);
    Get_store(_size) -> _dstring;
    dstring16_key -> _dstring!KEY;
    _len -> _dstring!V_LENGTH;
    ;;; ensure any padding bytes at the end of both parts are zero
    _0 -> _dstring@(w)[_-1]!(w){_STRING16_LIM_OFFS(_len)};
    _0 -> _dstring@POPBASE[_-1]!(w){_size}
enddefine;

define initdstring(_len) -> dstring;
    lvars dstring, _len;
    Check_integer(_len, 0);
    _int(_len) -> _len;
    Get_dstring(_len) -> dstring;
    ;;; fill all bytes with zeros
    _fill(_0, @@POPBASE{_DSTRING_SIZE(_len)} _sub @@V_WORDS, dstring@V_WORDS)
enddefine;

define initdstring16(_len) -> dstring;
    lvars dstring, _len;
    Check_integer(_len, 0);
    _int(_len) -> _len;
    Get_dstring16(_len) -> dstring;
    ;;; fill all bytes with zeros
    _fill(_0, @@POPBASE{_DSTRING16_SIZE(_len)} _sub @@V_WORDS, dstring@V_WORDS)
enddefine;

define Fill_dstring(dstring, _do_check) -> dstring;
    lvars   dstring, _cptr, _aptr, _clim, _dchar, _len = dstring!V_LENGTH,
            _do_check, _flags = dstring!KEY!K_FLAGS;

    unless _flags _bitst _:M_K_DSTRING then
        chain(dstring, _do_check, Fill_string)
    endunless;

    if _flags _bitst _:M_K_STRING16 then
        dstring@V_SHORTS[_0] -> _clim;
        _clim@(s)[_len] -> _cptr;
        _DSTRING16_ATTR_PTR(dstring, _len) -> _aptr;
        if _do_check then
            while _cptr >@(s) _clim do
                _int(Checkr_dchar()) ->> _dchar -> _cptr--!(s) -> _cptr;
                _shift(_dchar, _-16) -> _aptr--!(b) -> _aptr
            endwhile;
        else
            while _cptr >@(s) _clim do
                _int() ->> _dchar -> _cptr--!(s) -> _cptr;
                _shift(_dchar, _-16) -> _aptr--!(b) -> _aptr
            endwhile
        endif
    else
        dstring@V_BYTES[_0] -> _clim;
        _clim@(b)[_len] -> _cptr;
        _DSTRING_ATTR_PTR(dstring, _len) -> _aptr;
        if _do_check then
            while _cptr >@(b) _clim do
                _int(Checkr_dchar8()) ->> _dchar -> _cptr--!(b) -> _cptr;
                _shift(_dchar, _-16) -> _aptr--!(b) -> _aptr
            endwhile;
        else
            while _cptr >@(b) _clim do
                _int() ->> _dchar -> _cptr--!(b) -> _cptr;
                _shift(_dchar, _-16) -> _aptr--!(b) -> _aptr
            endwhile
        endif
    endif
enddefine;

define consdstring(_len) -> string;
    lvars   string, need16 = false, needd = false,
            _cptr, _uptr, _c, _clim, _x, _len;

    if iscompound(_len) then
        _len!KEY -> _x;
        if _x!K_FLAGS _bitst _:M_K_STRING then
            ;;; (for backward compatibility)
            _len;
            dstring_key -> _len, key_key -> _x
        endif;
        if _x == boolean_key then
            ;;; (for backward compatibility)
            unless _len then true ->> need16 -> needd endunless;
            () -> _len
        elseif _x == key_key
        and (_len!K_FLAGS ->> _x) _bitst _:M_K_STRING then
            ;;; extra key arg
            if _x _bitst _:M_K_STRING16 then true -> need16 endif;
            if _x _bitst _:M_K_DSTRING then true -> needd endif;
            _len -> _c;             ;;; save key
            if isstring(() ->> _len) then
                ;;; return appropriate representation
                _len -> string;
                string!KEY!K_FLAGS -> _x;
                if (needd and not(_x _bitst _:M_K_DSTRING)
                    or need16 and not(_x _bitst _:M_K_STRING16))
                and _nonzero(string!V_LENGTH ->> _len) then
                    subdstring(1, _pint(_len), string, _c) -> string
                endif;
                return
            endif
        endif
    endif;

    unless isinteger(_len) and _len fi_>= 0 then
        Check_integer(_len, 0)
    endunless;
    _int(_len) ->> _x -> _len;

    _user_sp() -> _uptr;
    until _zero(_x) do
        _x _sub _1 -> _x;
        _uptr!(w)++ -> (_c, _uptr);
        if isinteger(_c) then
            _int(_c) -> _c;
            if _c _bitst _16:FF0000 then true -> needd endif;
            nextunless(_c _bitst _:INV8);
            true -> need16;
            nextunless(_c _bitst _:INV16);
            _pint(_c) -> _c
        endif;
        Checkr_dchar(_c) ->     ;;; mishap
    enduntil;

    if need16 then
        if needd then
            Get_dstring16(_len) -> string;
            string@V_SHORTS[_0] -> _clim;
            _clim@(s)[_len] -> _cptr;
            _DSTRING16_ATTR_PTR(string, _len) -> _uptr;
            while _cptr >@(s) _clim do
                _int() ->> _c -> _cptr--!(s) -> _cptr;
                _shift(_c, _-16) -> _uptr--!(b) -> _uptr
            endwhile
        else
            Get_string16(_len) -> string;
            string@V_SHORTS[_0] -> _clim;
            _clim@(s)[_len] -> _cptr;
            while _cptr >@(s) _clim do
                _int() -> _cptr--!(s) -> _cptr
            endwhile
        endif
    else
        if needd then
            Get_dstring(_len) -> string;
            string@V_BYTES[_0]  -> _clim;
            _clim@(b)[_len] -> _cptr;
            _DSTRING_ATTR_PTR(string, _len) -> _uptr;
            while _cptr >@(b) _clim do
                _int() ->> _c -> _cptr--!(b) -> _cptr;
                _shift(_c, _-16) -> _uptr--!(b) -> _uptr
            endwhile
        else
            Get_string(_len) -> string;
            string@V_BYTES[_0]  -> _clim;
            _clim@(b)[_len] -> _cptr;
            while _cptr >@(b) _clim do
                _int() -> _cptr--!(b) -> _cptr
            endwhile
        endif
    endif
enddefine;

define destdstring(dstring);
    lvars dstring, _indx, _aoffs, _len, _flags;
    Check_string(dstring);
    dstring!KEY!K_FLAGS -> _flags;
    returnunless(_flags _bitst _:M_K_DSTRING) (Dest_string(dstring));

    dstring!V_LENGTH -> _len;
    _0 -> _indx;
    if _flags _bitst _:M_K_STRING16 then
        _STRING16_LIM_OFFS(_len) -> _aoffs;
        while _indx _lt _len do
            _CHECKUSER;
            _pint( _shift(dstring@(w){_aoffs}!(w->b)[_indx], _16)
                                        _add dstring!(w->s)[_indx] );
            _indx _add _1 -> _indx
        endwhile
    else
        _BYTEVEC_LIM_OFFS(_len) -> _aoffs;
        while _indx _lt _len do
            _CHECKUSER;
            _pint( _shift(dstring@(w){_aoffs}!(w->b)[_indx], _16)
                                        _add dstring!(w->b)[_indx] );
            _indx _add _1 -> _indx
        endwhile
    endif;
    _pint(_len)
enddefine;

define fast_subscrdstring(_subs, dstring);
    lvars dstring, _subs = _int(_subs) _sub _1, _flags = dstring!KEY!K_FLAGS;
    if _flags _bitst _:M_K_STRING16 then
        dstring!V_SHORTS[_subs];
        if _flags _bitst _:M_K_DSTRING then
            () _add _shift(_DSTRING16_ATTR_CHAR(dstring, _subs), _16)
        endif
    else
        dstring!V_BYTES[_subs];
        if _flags _bitst _:M_K_DSTRING then
            () _add _shift(_DSTRING_ATTR_CHAR(dstring, _subs), _16)
        endif
    endif;
    _pint()
enddefine;
;;;
define updaterof fast_subscrdstring(/*_dchar,*/ _subs, dstring) with_nargs 3;
    lvars   dstring, _dchar, _subs = _int(_subs) _sub _1,
            _flags = dstring!KEY!K_FLAGS, _attr;
    if _flags _bitst _:M_K_STRING16 then
        _int(Checkr_dchar()) ->> _dchar -> dstring!V_SHORTS[_subs];
        _shift(_dchar, _-16) -> _attr;
        if _flags _bitst _:M_K_DSTRING then
            _attr -> _DSTRING16_ATTR_CHAR(dstring, _subs);
            return
        endif
    else
        _int(Checkr_dchar8()) ->> _dchar -> dstring!V_BYTES[_subs];
        _shift(_dchar, _-16) -> _attr;
        if _flags _bitst _:M_K_DSTRING then
            _attr -> _DSTRING_ATTR_CHAR(dstring, _subs);
            return
        endif
    endif;
    ;;; non-dstring
    if _nonzero(_attr) then
        mishap(_pint(_dchar), _pint(_subs _add _1), dstring, 3,
                    'ASSIGNING NON-ZERO ATTRIBUTE CHAR INTO ORDINARY STRING')
    endif
enddefine;

define subscrdstring(_subs, dstring);
    lvars dstring, _subs;
    Check_string(dstring);
    Check_vsubscr(_subs, dstring);
    fast_subscrdstring(_subs, dstring)
enddefine;
;;;
define updaterof subscrdstring(_subs, dstring) with_nargs 3;
    lvars _subs, dstring;
    Check_string(dstring);
    Check_vsubscr(_subs, dstring);
    () -> fast_subscrdstring(_subs, dstring)
enddefine;


;;; --- DSTRING UTILITIES ---------------------------------------------------

define subdstring(_ssub, _len, dstring) /* -> subdstr */;
    lvars subdstr, dstring, _ssub, _len, _saptr,
            getd8_p = Get_dstring, _dopt = true, _flags;

    if iscompound(dstring) then
        dstring!KEY -> _flags;
        if _flags == boolean_key then
            ;;; extra arg
            unless dstring then false ->> getd8_p -> _dopt endunless;
            ((), _ssub, _len) -> (_ssub, _len, dstring)
        elseif _flags == key_key
        and (dstring!K_FLAGS ->> _flags) _bitst _:M_K_STRING then
            ;;; extra arg
            ((), _ssub, _len) -> (_ssub, _len, dstring);
            if _flags _bitst _:M_K_STRING16 then false -> getd8_p endif;
            if _flags _bitst _:M_K_DSTRING then false -> _dopt endif
        endif
    endif;
    if isword(dstring) then dstring!W_STRING -> dstring endif;

    Check_integer(_len, 0);
    _int(_len) -> _len;
    Check_bytevec_subrange(_ssub, dstring, _len);
    returnif(_zero(_len)) (nullstring);
    _int(_ssub) _sub _1 -> _ssub;

    dstring!KEY!K_FLAGS -> _flags;
    returnif(_dopt and not(_flags _bitst _:M_K_DSTRING))
        (Substring(_ssub, dstring, _len, getd8_p and Get_string, Get_string16));

    Substring(_ssub, dstring, _len, getd8_p, Get_dstring16) -> subdstr;

    if subdstr!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        _DSTRING16_ATTR_PTR(subdstr, _0) -> _saptr
    else
        _DSTRING_ATTR_PTR(subdstr, _0) -> _saptr
    endif;
    if _flags _bitst _:M_K_DSTRING then
        ;;; copy in the attributes
        _bmove(@@(b)[_len],
                if _flags _bitst _:M_K_STRING16 then
                    _DSTRING16_ATTR_PTR(dstring, _ssub)
                else
                    _DSTRING_ATTR_PTR(dstring, _ssub)
                endif, _saptr) -> ;
        if _dopt then subdstr -> Get_dstring(_len, false) endif
    else
        ;;; zero attributes
        _bfill(_0, @@(b)[_len], _saptr)
    endif;
    subdstr
enddefine;
;;;
define updaterof subdstring(subdstr, _ssub, _len, dstring);
    lvars subdstr, dstring, _saptr, _salim, _aptr, _flags, _ssub, _len;
    if isword(subdstr) then subdstr!W_STRING -> subdstr endif;

    ;;; move the basic chars
    subdstr -> substring(_ssub, _len, dstring);

    _int(_ssub) _sub _1 -> _ssub;
    _int(_len) -> _len;

    if (dstring!KEY!K_FLAGS ->> _flags) _bitst _:M_K_DSTRING then
        if _flags _bitst _:M_K_STRING16 then
            _DSTRING16_ATTR_PTR(dstring, _ssub) -> _aptr
        else
            _DSTRING_ATTR_PTR(dstring, _ssub) -> _aptr
        endif;
        if (subdstr!KEY!K_FLAGS ->> _flags) _bitst _:M_K_DSTRING then
            ;;; copy in the attributes
            if _flags _bitst _:M_K_STRING16 then
                _DSTRING16_ATTR_PTR(subdstr, _0) -> _saptr
            else
                _DSTRING_ATTR_PTR(subdstr, _0) -> _saptr
            endif;
            _bmove(@@(b)[_len], _saptr, _aptr) ->
        else
            ;;; zero the attributes
            _bfill(_0, @@(b)[_len], _aptr)
        endif
    elseif (subdstr!KEY!K_FLAGS ->> _flags) _bitst _:M_K_DSTRING then
        ;;; check attributes in subdstr are zero
        if _flags _bitst _:M_K_STRING16 then
            _DSTRING16_ATTR_PTR(subdstr, _0) -> _saptr
        else
            _DSTRING_ATTR_PTR(subdstr, _0) -> _saptr
        endif;
        _saptr@(b)[_len] -> _salim;
        while _saptr <@(b) _salim do
            if _nonzero(_saptr!(b)++ -> _saptr) then
                mishap(subdstr, dstring, 2,
                    'ASSIGNING NON-ZERO ATTRIBUTE DSTRING INTO ORDINARY STRING')
            endif
        endwhile
    endif
enddefine;


;;; --- DSTRING_KEY -------------------------------------------------------

constant
    dstring_key = struct KEY_V =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_VECTOR
            _biset _:M_K_STRING
            _biset _:M_K_DSTRING
            _biset _:M_K_BYTE_ACCESS
            _biset _:M_K_COPY,  ;;; K_FLAGS
        _:GCTYPE_USERNFVEC,     ;;; K_GC_TYPE
        procedure(); _DSTRING_SIZE(()!V_LENGTH) endprocedure,
                                ;;; K_GET_SIZE

        "dstring",              ;;; K_DATAWORD
        24,                     ;;; K_SPEC
        isdstring,              ;;; K_RECOGNISER
        WREF Vector_apply,      ;;; K_APPLY
        Eq__String8,            ;;; K_SYS_EQUALS
        WREF Eq__String8,       ;;; K_EQUALS
        Str_print,              ;;; K_SYS_PRINT
        WREF Str_print,         ;;; K_PRINT
        WREF Str_hash,          ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        _:t_BYTE,               ;;; K_FIELD_CODE_V
        initdstring,            ;;; K_INIT_V
        consdstring,            ;;; K_CONS_V
        destdstring,            ;;; K_DEST_V
        subscrdstring,          ;;; K_SUBSCR_V
        fast_subscrdstring      ;;; K_FAST_SUBSCR_V
        %},

    dstring16_key = struct KEY_V =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_VECTOR
            _biset _:M_K_STRING
            _biset _:M_K_STRING16
            _biset _:M_K_DSTRING
            _biset _:M_K_BYTE_ACCESS
            _biset _:M_K_COPY,  ;;; K_FLAGS
        _:GCTYPE_USERNFVEC,     ;;; K_GC_TYPE
        procedure(); lvars _l = ()!V_LENGTH; _DSTRING16_SIZE(_l) endprocedure,
                                ;;; K_GET_SIZE

        "dstring16",            ;;; K_DATAWORD
        24,                     ;;; K_SPEC
        isdstring,              ;;; K_RECOGNISER
        WREF Vector_apply,      ;;; K_APPLY
        Eq__String16,           ;;; K_SYS_EQUALS
        WREF Eq__String16,      ;;; K_EQUALS
        Str_print,              ;;; K_SYS_PRINT
        WREF Str_print,         ;;; K_PRINT
        WREF Str_hash,          ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        _:t_SHORT,              ;;; K_FIELD_CODE_V
        initdstring16,          ;;; K_INIT_V
        consdstring,            ;;; K_CONS_V
        destdstring,            ;;; K_DEST_V
        subscrdstring,          ;;; K_SUBSCR_V
        fast_subscrdstring      ;;; K_FAST_SUBSCR_V
        %};


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  8 1997
        Fixed stupid bug in updater of subdstring (_len wasn't converted to
        sysint)
--- John Gibson, Feb  4 1997
        String16 changes
--- John Gibson, Sep  4 1995
        Changed updater of fast_subscrdstring to mishap when assigning char
        with nonzero attributes into ordinary string
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Mar 26 1992
        Changed K_SPEC field to 24 rather than "ushort"
--- John Gibson, Mar 15 1992
        Attributes now stored in bits 16-23 in integer characters
 */
