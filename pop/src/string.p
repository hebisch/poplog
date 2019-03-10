/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/string.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *STRINGS
 */

;;; ----------------------- STRINGS --------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'gctypes.ph'

constant
        _scmp
    ;

section $-Sys;

constant
        procedure (Bytevec_hashint, Bytevec_getsize, Vector_apply,
        Extern$-Get_encoding_funcs)
    ;

endsection;


;;; ------------------------------------------------------------------------


section $-Sys => nullstring, isstring, isstring16, inits, inits16,
                 consstring, deststring, subscrs, fast_subscrs, substring,
                 isstartstring, issubstring, pop_sys_encoding,
                 sys_decode_string, sys_encode_string,
                 string_key, string16_key;

vars
    sys_encoding = false;       ;;; N.B. not restored
    ;


lconstant macro (
    INV8    = ~~16:FF00FF,
    INV16   = ~~16:FFFFFF,
);


define active pop_sys_encoding;
    sys_encoding and sys_encoding!XP_PROPS
enddefine;
;;;
define updaterof pop_sys_encoding encoding_name;
    if encoding_name and Extern$-Get_encoding_funcs(encoding_name)
                                    ->> sys_encoding
    then
        sys_encoding!XP_PTR
    else
        _NULL
    endif -> _extern _pop_sys_encoding_funcs:data!((struct CODING_FUNCS))
enddefine;

define Check_string(_item);
    lvars _item;
    unless isstring(_item) then
        mishap(_item, 1, 'STRING NEEDED')
    endunless
enddefine;

define Check_string8(_item);
    lvars _item, _flags;
    unless iscompound(_item)
    and (_item!KEY!K_FLAGS ->> _flags) _bitst _:M_K_STRING
    and not(_flags _bitst _:M_K_STRING16) then
        mishap(_item, 1, '8-BIT STRING NEEDED')
    endunless
enddefine;

define Checkr_byte(_item) -> _item;
    lvars _item;
    unless isinteger(_item) and _int(_item) _lteq _16:FF then
        mishap(_item, 1, 'INTEGER BYTE VALUE NEEDED')
    endunless;
enddefine;

define Checkr_dchar8(_item) -> _item;
    lvars _item;
    unless isinteger(_item) and not(_int(_item) _bitst _:INV8) then
        mishap(_item, 1, 'INTEGER (8-BIT) CHARACTER NEEDED')
    endunless
enddefine;

define Checkr_dchar(_item) -> _item;
    lvars _item;
    unless isinteger(_item) and not(_int(_item) _bitst _:INV16) then
        mishap(_item, 1, 'INTEGER CHARACTER NEEDED')
    endunless
enddefine;

define isstring(_item);
    lvars _item;
    iscompound(_item) and _item!KEY!K_FLAGS _bitst _:M_K_STRING
enddefine;

define isstring16(_item);
    lvars _item;
    iscompound(_item) and _item!KEY!K_FLAGS _bitst _:M_K_STRING16
enddefine;

define Get_string(_len) -> string;
    lvars string, _len, _size = _BYTEVEC_SIZE(_len);
    Get_store(_size) -> string;
    string_key -> string!KEY;
    _len -> string!V_LENGTH;
    ;;; ensure any padding bytes at the end are zero
    _0 -> string@POPBASE[_-1]!(w){_size}
enddefine;
;;;
define updaterof Get_string(string, _len);
    lvars string, _bptr, _blim, _len, _offs;
    _len -> string!V_LENGTH;
    ;;; return unused space and pad string with zeros correctly
    string@(w){_BYTEVEC_SIZE(_len)} ->> _blim -> Get_store();
    _blim@POPBASE@(w->b) -> _blim;
    string@V_BYTES[_len] -> _bptr;
    while _bptr <@(b) _blim do _0 -> _bptr!(b)++ -> _bptr endwhile
enddefine;

define Get_string16(_len) -> string;
    lvars string, _len, _size = _STRING16_SIZE(_len);
    Get_store(_size) -> string;
    string16_key -> string!KEY;
    _len -> string!V_LENGTH;
    ;;; ensure any padding shorts at the end are zero
    _0 -> string@POPBASE[_-1]!(w){_size}
enddefine;

define Fill_string(string, _do_check) -> string;
    lvars string, _cptr, _clim, _do_check;
    if string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        string@V_SHORTS[_0] -> _clim;
        _clim@(s)[string!V_LENGTH] -> _cptr;
        if _do_check then
            while _cptr >@(s) _clim do
                _int(Checkr_dchar()) -> _cptr--!(s) -> _cptr
            endwhile;
        else
            while _cptr >@(s) _clim do
                _int() -> _cptr--!(s) -> _cptr
            endwhile
        endif
    else
        string@V_BYTES[_0] -> _clim;
        _clim@(b)[string!V_LENGTH] -> _cptr;
        if _do_check then
            while _cptr >@(b) _clim do
                _int(Checkr_dchar8()) -> _cptr--!(b) -> _cptr
            endwhile;
        else
            while _cptr >@(b) _clim do
                _int() -> _cptr--!(b) -> _cptr
            endwhile
        endif
    endif
enddefine;

define Dest_string(string);
    lvars string, _coffs, _clim;
    if string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        @@V_SHORTS -> _coffs;
        @@V_SHORTS[string!V_LENGTH] -> _clim;
        while _coffs _lt _clim do
            _CHECKUSER;
            _pint( string!(w->s){_coffs} );
            @@(s){_coffs}++ -> _coffs
        endwhile
    else
        @@V_BYTES -> _coffs;
        @@V_BYTES[string!V_LENGTH] -> _clim;
        while _coffs _lt _clim do
            _CHECKUSER;
            _pint( string!(w->b){_coffs} );
            @@(b){_coffs}++ -> _coffs
        endwhile
    endif;
    _pint(string!V_LENGTH)
enddefine;

define deststring(string);
    lvars string;
    Check_string(string);
    Dest_string(string)
enddefine;

define subscrs(_subs, string);
    lvars _subs, string, _flags;
    unless iscompound(string)
    and (string!KEY!K_FLAGS ->> _flags) _bitst _:M_K_STRING then
        Check_string(string)
    endunless;
    Check_vsubscr(_subs, string);
    _int(_subs) -> _subs;
    if _flags _bitst _:M_K_STRING16 then
        _pint(string!V_SHORTS[_subs _sub _1])
    else
        _pint(string!V_BYTES[_subs _sub _1])
    endif
enddefine;
;;;
define updaterof subscrs(_subs, string) with_nargs 3;
    lvars _subs, string, _flags;
    unless iscompound(string)
    and (string!KEY!K_FLAGS ->> _flags) _bitst _:M_K_STRING then
        Check_string(string)
    endunless;
    Check_vsubscr(_subs, string);
    _int(_subs) -> _subs;
    if _flags _bitst _:M_K_STRING16 then
        _int(Checkr_dchar()) -> string!V_SHORTS[_subs _sub _1]
    else
        _int(Checkr_dchar8()) -> string!V_BYTES[_subs _sub _1]
    endif
enddefine;

define fast_subscrs(_subs, string);
    lvars _subs = _int(_subs), string;
    if string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        _pint(string!V_SHORTS[_subs _sub _1])
    else
        _pint(string!V_BYTES[_subs _sub _1])
    endif
enddefine;
;;;
define updaterof fast_subscrs(_subs, string) with_nargs 3;
    lvars _subs = _int(_subs), string;
    if string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        _int(Checkr_dchar()) -> string!V_SHORTS[_subs _sub _1]
    else
        _int(Checkr_dchar8()) -> string!V_BYTES[_subs _sub _1]
    endif
enddefine;


;;; --- STRING UTILITIES ---------------------------------------------------

define Check_bytevec_subrange(_bsub, bvec, _len);
    lvars _bsub, bvec, _len;
    Check_bytevec(bvec);
    Check_integer(_bsub, 1);
    unless _int(_bsub) _add _len _sub _1 _lteq bvec!V_LENGTH then
        mishap(_bsub, bvec, _pint(_len), 3,
                                'SUB RANGE EXCEEDS STRING/VECTOR LENGTH')
    endunless;
enddefine;

define :inline lconstant SUBSTR16(_SPTR, result);
    if get8_p then
        _SPTR -> _sptr;
        _sptr@(s)[_len] -> _lim;
        while _sptr <@(s) _lim do
            if (_sptr!(s)++ -> _sptr) _gr _:16:FF then
                false -> get8_p;
                quitloop
            endif
        endwhile
    endif;
    if get8_p then
        fast_apply(_len, get8_p) -> result;
        result@V_BYTES[_0] -> _bptr;
        _SPTR -> _sptr;
        _sptr@(s)[_len] -> _lim;
        while _sptr <@(s) _lim do
            (_sptr!(s)++ -> _sptr) -> _bptr!(b)++ -> _bptr
        endwhile
    else
        fast_apply(_len, get16_p) -> result;
        _smove(@@(s)[_len], _SPTR, result@V_SHORTS[_0]) ->
    endif
enddefine;

define Substring(_sindx, string, _len, get8_p, get16_p) -> substr;
    lvars string, substr, get8_p, get16_p, _sptr, _bptr, _lim, _len, _sindx;
    if string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        SUBSTR16(string@V_SHORTS[_sindx], substr)
    else
        if get8_p then
            fast_apply(_len, get8_p) -> substr;
            _bmove(@@(b)[_len], string@V_BYTES[_sindx], substr@V_BYTES[_0]) ->
        else
            fast_apply(_len, get16_p) -> substr;
            substr@V_SHORTS[_0] -> _sptr;
            string@V_BYTES[_sindx] -> _bptr;
            _bptr@(b)[_len] -> _lim;
            while _bptr <@(b) _lim do
                (_bptr!(b)++ -> _bptr) -> _sptr!(s)++ -> _sptr
            endwhile
        endif
    endif
enddefine;

define substring(_ssub, _len, string) /* -> substr */;
    lvars substr, string, _ssub, _len, get8_p = Get_string;
    if iskey(string) then
        if string == string16_key then
            false -> get8_p
        elseunless string == string_key then
            mishap(string, 1, 'INVALID KEY FOR substring')
        endif;
        (), _ssub, _len -> (_ssub, _len, string)
    endif;

    if isword(string) then string!W_STRING -> string endif;
    Check_integer(_len, 0);
    _int(_len) -> _len;
    Check_bytevec_subrange(_ssub, string, _len);
    returnif(_zero(_len)) (nullstring);
    _int(_ssub) _sub _1 -> _ssub;
    Substring(_ssub, string, _len, get8_p, Get_string16)
enddefine;
;;;
define updaterof substring(substr, _ssub, _len, string);
    lvars substr, string, _bptr, _sptr, _len, _ssub;
    if isword(substr) then substr!W_STRING -> substr endif;
    Check_integer(_len, 0);
    _int(_len) -> _len;
    Check_bytevec_subrange(_ssub, string, _len);
    Check_bytevec_subrange(1, substr, _len);
    _int(_ssub) _sub _1 -> _ssub;
    if string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        if substr!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            _smove(@@(s)[_len], substr@V_SHORTS[_0], string@V_SHORTS[_ssub]) ->
        else
            substr@V_BYTES[_0] -> _bptr;
            _bptr@(b)[_len] -> _len;
            string@V_SHORTS[_ssub] -> _sptr;
            while _bptr <@(b) _len do
                (_bptr!(b)++ -> _bptr) -> _sptr!(s)++ -> _sptr
            endwhile
        endif
    else
        if substr!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            substr@V_SHORTS[_0] -> _sptr;
            _sptr@(s)[_len] -> _len;
            string@V_BYTES[_ssub] -> _bptr;
            while _sptr <@(s) _len do
                _sptr!(s)++ -> (_ssub, _sptr);
                if _ssub _gr _16:FF then
                    mishap(substr, string, 2,
                            'ASSIGNING 16-BIT STRING CHARS INTO 8-BIT STRING')
                endif;
                _ssub -> _bptr!(b)++ -> _bptr
            endwhile
        else
            _bmove(@@(b)[_len], substr@V_BYTES[_0], string@V_BYTES[_ssub]) ->
        endif
    endif
enddefine;

define Bcmp16(_boffs, _str8p, _str16p);
    lvars _str8p, _str16p, _boffs;
    _str8p@(b){_boffs} -> _boffs;
    while _str8p <@(b) _boffs do
        returnif( (_str8p!(b)++ -> _str8p) /== (_str16p!(s)++ -> _str16p) )
                    (false)
    endwhile;
    true
enddefine;

define isstartstring(substr, string);
    lvars substr, string, _sublen;
    if isword(string) then
        string!W_STRING -> string
    else
        Check_bytevec(string)
    endif;
    if isword(substr) then
        substr!W_STRING -> substr
    else
        Check_bytevec(substr)
    endif;
    substr!V_LENGTH -> _sublen;
    returnunless(string!V_LENGTH _greq _sublen) (false);
    if string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        if substr!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            _scmp(@@(s)[_sublen], string@V_SHORTS, substr@V_SHORTS)
        else
            Bcmp16(@@(b)[_sublen], substr@V_BYTES, string@V_SHORTS)
        endif
    else
        if substr!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            Bcmp16(@@(b)[_sublen], string@V_BYTES, substr@V_SHORTS)
        else
            _bcmp(@@(b)[_sublen], string@V_BYTES, substr@V_BYTES)
        endif
    endif;
    () and 1
enddefine;

define Explode_substring(_ssub, _len, string);
    lvars string, _coffs, _len, _ssub;
    if isword(string) then string!W_STRING -> string endif;
    Check_integer(_len, 0);
    Check_bytevec_subrange(_ssub, string, _int(_len));
    if string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        @@V_SHORTS[_int(_ssub) _sub _1] -> _coffs;
        until _len == 0 do
            _CHECKUSER;
            _pint( string!(w->s){_coffs} );
            @@(s){_coffs}++ -> _coffs;
            _len fi_- 1 -> _len
        enduntil
    else
        @@V_BYTES[_int(_ssub) _sub _1] -> _coffs;
        until _len == 0 do
            _CHECKUSER;
            _pint( string!(w->b){_coffs} );
            @@(b){_coffs}++ -> _coffs;
            _len fi_- 1 -> _len
        enduntil
    endif
enddefine;

    /*  Saves using the generic allbutfirst
    */
define Str_allbutfirst(_n, string);
    lvars string, _n;
    substring(_n fi_+ 1, datalength(string) fi_- _n, string)
enddefine;


    /*  Return chars of string from _ssub to end as a (radix 10) system
        integer, or false if contains non-digits.
        (Avoids using strnumber, which brings in the whole of the itemiser).
    */
define Endstring_to_num(_ssub, string);
    lvars string, _ptr, _lim, _d, _ssub = _int(_ssub) _sub _1, _n = _0;
    if string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        string@V_SHORTS[string!V_LENGTH] -> _lim;
        string@V_SHORTS[_ssub] -> _ptr;
        returnunless(_ptr <@(s) _lim) (false);
        repeat
            (_ptr!(s)++ -> _ptr) _sub _:`0` -> _d;
            returnunless(_0 _lteq _d and _d _lt _10)(false);
            _n _add _d -> _n;
            returnif(_ptr == _lim)(_n);
            _n _mult _10 -> _n;
        endrepeat;
    else
        string@V_BYTES[string!V_LENGTH] -> _lim;
        string@V_BYTES[_ssub] -> _ptr;
        returnunless(_ptr <@(b) _lim) (false);
        repeat
            (_ptr!(b)++ -> _ptr) _sub _:`0` -> _d;
            returnunless(_0 _lteq _d and _d _lt _10)(false);
            _n _add _d -> _n;
            returnif(_ptr == _lim)(_n);
            _n _mult _10 -> _n;
        endrepeat;
    endif
enddefine;


#_IF DEF VMS
constant
    sysstring   = writeable inits(SYSSTRING_LENGTH);

define Copy_sysstring();
    substring(1, _pint(_sysstring_len), sysstring)
enddefine;
#_ENDIF


;;; --- DECODING/ENCODING ----------------------------------------------

define Consstring_bptr(_bptr, _len, _ptrmode) -> string;
    lvars   string, get8_p, get16_p, _sptr, _bptr, _lim, _len, _ptrmode,
            _decode = sys_encoding;
    lstackmem int _ilenp, int _olenp, int _state, s_stackbuff _sbuf;

    define lconstant long_output(_bptr, _len, _ptrmode, _decode);
        lvars tmp, _string, _bptr, _len, _ptrmode, _decode;
        lstackmem int _ilenp2, int _olenp2, int _state2;
        _CLAWBACK_SAVE;
        if _ptrmode == CSB_LSTACKMEM then
            ;;; relativise the lstackmem pointer in case of process suspension
            ;;; during a GC interrupt
            @@(csbyte){_bptr, _sp()@(csword->csbyte)} -> _bptr;
            Get_string16(_len);
            _sp()@(csword->csbyte){_bptr} -> _bptr
        elseif _ptrmode == CSB_POP_STRING then
            ;;; _bptr is a pop string
            _bptr -> tmp;
            Get_string16(_len);
            tmp -> _bptr
        else
            Get_string16(_len)
        endif -> _string;
        _len ->> _ilenp2!(i) -> _olenp2!(i);
        _0 -> _state2!(i);
        _extern[INDIR] _decode(_bptr, _ilenp2, _string@V_SHORTS, _olenp2,
                                                        _state2, _0) -> ;
        Clawback(Substring(_0, _string, _olenp2!(i), Get_string, Get_string16))
    enddefine;

    unless isinteger(_ptrmode) then
        ;;; optional exptr encoding arg (or false)
        (), _bptr, _len, _ptrmode -> (_bptr, _len, _ptrmode, _decode)
    endunless;

    returnif(_bptr == _NULL) (termin -> string);
    if _neg(_len) then
        ;;; null-terminated
        _bptr -> _sptr;
        until _zero(_sptr!(b)++ -> _sptr) do enduntil;
        ##(b){_sptr--@(b), _bptr} -> _len
    endif;

    if _decode then
        _decode!XP_PTR!CDFN_DECODE -> _decode;
        _len -> _ilenp!(i);
        ##(s)[_1|s_stackbuff] -> _olenp!(i);
        _0 -> _state!(i);
        _extern[INDIR] _decode(_bptr, _ilenp, _sbuf, _olenp, _state, _0) -> ;
        if _zero(_ilenp!(i)) then
            ;;; everything used up
            _olenp!(i) -> _len;
            Get_string -> get8_p;
            Get_string16 -> get16_p;
            SUBSTR16(_sbuf, string)
        else
            ;;; assume s_stackbuff length is not enough (but assume _len is)
            chain(_bptr, _len, _ptrmode, _decode, long_output)
        endif
    else
        ;;; (can't be CSB_POP_STRING in this case)
        if _ptrmode == CSB_LSTACKMEM then
            ;;; relativise the lstackmem pointer in case of process suspension
            ;;; during a GC interrupt
            @@(csbyte){_bptr, _sp()@(csword->csbyte)} -> _bptr;
            Get_string(_len);
            _sp()@(csword->csbyte){_bptr} -> _bptr
        else
            Get_string(_len)
        endif -> string;
        _bmove(@@(b)[_len], _bptr, string@V_BYTES[_0]) ->
    endif
enddefine;

define sys_decode_string(string);
    lvars string, decode = sys_encoding;
    if isword(string) then
        ;;; optional encoding name
        Extern$-Get_encoding_funcs(string) -> (string, decode)
    elseunless string then
        ;;; false for encoding
        (), false -> (string, decode)
    endif;
    Check_string8(string);
    if decode then
        Consstring_bptr(string, string!V_LENGTH, CSB_POP_STRING, decode)
    else
        string
    endif
enddefine;

define Try_encode_string(string, _bptr, _olen, _encode)
                                                -> (was_enough, _nbytes);
    lvars   string, was_enough = true, _bptr, _sptr, _lim,
            _len = string!V_LENGTH, _olen, _c, _encode_flags, _nbytes, _encode;
    lstackmem int _ilenp, int _olenp, int _state;

    if _encode then
        _encode!XP_PTR!CDFN_ENCODE -> _encode;
        _0 -> _nbytes;
        _len -> _ilenp!(i);
        if string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            string@V_SHORTS[_len] -> _lim;
            _0 -> _encode_flags
        else
            string@V_BYTES[_len] -> _lim;
            _BYTE_INPUT -> _encode_flags
        endif;
        _0 -> _state!(i);

        repeat
            _olen -> _olenp!(i);
            _extern[INDIR] _encode( if _zero(_encode_flags) then
                                        _lim@(s)-[_len]
                                    else
                                        _lim@(b)-[_len]
                                    endif, _ilenp,
                                    _bptr, _olenp, _state, _encode_flags) -> ;
            _olenp!(i) _add _nbytes -> _nbytes;
            _ilenp!(i) -> _len;
            quitif(_zero(_len));
            false -> was_enough
        endrepeat

    elseif (_len ->> _nbytes) _lteq _olen then
        if string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            string@V_SHORTS[_0] -> _sptr;
            _sptr@(s)[_len] -> _lim;
            while _sptr <@(s) _lim do
                _sptr!(s)++ -> (_c, _sptr);
                if _c _gr _16:FF then
                    mishap(string, 1, '16-BIT STRING CHARS INVALID WITH NO CHAR ENCODING')
                endif;
                _c -> _bptr!(b)++ -> _bptr
            endwhile
        else
            _bmove(@@(b)[_len], string@V_BYTES, _bptr) ->
        endif

    else
        false -> was_enough
    endif
enddefine;

    /*  N.B. This procedure is called by sys_encode_string_fixed
        with fixed alloc set (hence it mustn't do more than one store alloc).
    */
define sys_encode_string(string) -> string;
    lvars string, new, encode = sys_encoding, _len, _nbytes, _was_enough;
    lconstant macro (
        BLEN = 936,
        WLEN = _pint(##(w){_BYTEVEC_DATA_SIZE(_:BLEN)})
    );
    lstackmem word _wbuf[WLEN];

    if isword(string) then
        ;;; optional encoding name
        Extern$-Get_encoding_funcs(string) -> (string, encode)
    elseunless string then
        ;;; false for encoding
        (), false -> (string, encode)
    endif;
    Check_string(string);
    if encode then
        string!V_LENGTH -> _len;
        if _len _lteq _:BLEN then
            ;;; ensure any padding bytes after _len will be zero in _wbuf,
            ;;; so the word _cmp below can work
            _0 -> _wbuf!(w){_BYTEVEC_DATA_SIZE(_len) _sub @@(w)++}
        endif;
        Try_encode_string(string, _wbuf@(w->b), _:BLEN, encode)
                                            -> (_was_enough, _nbytes);
        if _was_enough then
            unless _len == _nbytes
            and not(string!KEY!K_FLAGS _bitst _:M_K_STRING16)
            and _cmp( @@(w)[_len | b.r], string@V_WORDS, _wbuf)
            then
                Get_string(_nbytes) -> string;
                _bmove(@@(b)[_nbytes], _wbuf@(w->b), string@V_BYTES) ->
            ;;; else encoding doesn't change string
            endunless
        else
            Get_string(_nbytes) -> new;
            Try_encode_string(string, new@V_BYTES, _nbytes, encode) -> (,);
            new -> string
        endif
    elseif string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        Substring(_0, string, string!V_LENGTH, Get_string, Get_string16)
                                                -> string;
        if string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            mishap(string, 1, '16-BIT STRING CHARS INVALID WITH NO CHAR ENCODING')
        endif
    endif
enddefine;


;;; --- STRING_KEY -------------------------------------------------------

define Str_hash() with_nargs 1;
    _pint(Bytevec_hashint())
enddefine;

define Str_print(string);
    lvars string;
    if pop_pr_quotes then
        cucharout(`'`), Print_str(string), cucharout(`'`)
    else
        Print_str(string)
    endif
enddefine;

define Eq__String16(item, s16);
    lvars item, s16, _flags;
    if item == s16 then
        true
    elseif issimple(item) then
        false
    elseif (item!KEY!K_FLAGS ->> _flags) _bitst _:M_K_STRING
    and item!V_LENGTH == s16!V_LENGTH
    then
        if _flags _bitst _:M_K_STRING16 then
            ;;; can compare by words because any padding shorts are
            ;;; guaranteed to be zero
            _cmp( @@(w)[item!V_LENGTH | s.r], item@V_WORDS, s16@V_WORDS)
        else
            Bcmp16(@@(b)[item!V_LENGTH], item@V_BYTES, s16@V_SHORTS)
        endif
    elseif _flags _bitst _:M_K_MATCH_VAR then
        fast_chain(s16, item, item!KEY!K_SYS_EQUALS)
    else
        false
    endif
enddefine;

define Eq__String8(item, s8);
    lvars item, s8, _flags;
    if item == s8 then
        true
    elseif issimple(item) then
        false
    elseif (item!KEY!K_FLAGS ->> _flags) _bitst _:M_K_STRING
    and item!V_LENGTH == s8!V_LENGTH
    then
        unless _flags _bitst _:M_K_STRING16 then
            ;;; can compare by words because any padding bytes are
            ;;; guaranteed to be zero
            _cmp( @@(w)[item!V_LENGTH | b.r], item@V_WORDS, s8@V_WORDS)
        else
            Bcmp16(@@(b)[s8!V_LENGTH], s8@V_BYTES, item@V_SHORTS)
        endunless
    elseif _flags _bitst _:M_K_MATCH_VAR then
        fast_chain(s8, item, item!KEY!K_SYS_EQUALS)
    else
        false
    endif
enddefine;

constant
    string_key = struct KEY_V =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_VECTOR
            _biset _:M_K_STRING
            _biset _:M_K_NO_FULL_FROM_PTR
            _biset _:M_K_BYTE_ACCESS
            _biset _:M_K_COPY,  ;;; K_FLAGS
        _:GCTYPE_BYTEVEC,       ;;; K_GC_TYPE
        Bytevec_getsize,        ;;; K_GET_SIZE

        "string",               ;;; K_DATAWORD
        "byte",                 ;;; K_SPEC
        isstring,               ;;; K_RECOGNISER
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
        inits,                  ;;; K_INIT_V
        consstring,             ;;; K_CONS_V
        deststring,             ;;; K_DEST_V
        subscrs,                ;;; K_SUBSCR_V
        fast_subscrs            ;;; K_FAST_SUBSCR_V
        %},

    ;;; Short name for this key used by poplink
    ;;; (saves space on the assembler output!)

    $-K$-s  = string_key,

    string16_key = struct KEY_V =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_VECTOR
            _biset _:M_K_STRING
            _biset _:M_K_STRING16
            _biset _:M_K_NO_FULL_FROM_PTR
            _biset _:M_K_BYTE_ACCESS
            _biset _:M_K_COPY,  ;;; K_FLAGS
        _:GCTYPE_USERNFVEC,     ;;; K_GC_TYPE

        procedure(); _STRING16_SIZE(()!V_LENGTH) endprocedure,
                                ;;; K_GET_SIZE

        "string16",             ;;; K_DATAWORD
        "ushort",               ;;; K_SPEC
        isstring,               ;;; K_RECOGNISER
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
        inits16,                ;;; K_INIT_V
        consstring,             ;;; K_CONS_V
        deststring,             ;;; K_DEST_V
        subscrs,                ;;; K_SUBSCR_V
        fast_subscrs            ;;; K_FAST_SUBSCR_V
        %},

    ;


;;; --- SPECIAL THINGS AT THE END --------------------------------------------
;;; N.B. Execute-level code that references these things afterwards
;;; will not get the normal values (i.e. so leave them as the last things
;;; in the file).

    /*  Standard empty string
    */
constant
    nullstring  = '';

define inits(_len) -> string;
    lvars string, _len, _size;
    Check_integer(_len, 0);
    _int(_len) -> _len;
    _BYTEVEC_SIZE(_len) -> _size;
    Get_store(_size) -> string;
    string_key -> string!KEY;
    _len -> string!V_LENGTH;
    ;;; fill bytes with zeros
    _fill(_0, @@POPBASE{_size} _sub @@V_WORDS, string@V_WORDS)
enddefine;

define inits16(_len) -> string;
    lvars string, _len, _size;
    Check_integer(_len, 0);
    _int(_len) -> _len;
    _STRING16_SIZE(_len) -> _size;
    Get_store(_size) -> string;
    string16_key -> string!KEY;
    _len -> string!V_LENGTH;
    ;;; fill bytes with zeros
    _fill(_0, @@POPBASE{_size} _sub @@V_WORDS, string@V_WORDS)
enddefine;

define consstring(_len) -> string;
    lvars string, need16 = false, _cptr, _uptr, _c, _clim, _x, _len;
    if iskey(_len) then
        if _len == string16_key then
            true -> need16
        elseunless _len == string_key then
            mishap(_len, 1, 'INVALID KEY FOR consstring')
        endif;
        if isstring(() ->> _len) then
            _len -> string;
            if need16 and not(string!KEY!K_FLAGS _bitst _:M_K_STRING16)
            and _nonzero(string!V_LENGTH ->> _len) then
                ;;; convert to a string16
                Substring(_0, string, _len, false, Get_string16) -> string
            endif;
            return
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
            nextunless(_c _bitst _:INV8);
            true -> need16;
            nextunless(_c _bitst _:INV16);
            _pint(_c) -> _c
        endif;
        Checkr_dchar(_c) ->     ;;; mishap
    enduntil;

    if need16 then
        Get_string16(_len) -> string;
        string@V_SHORTS[_0] -> _clim;
        _clim@(s)[_len] -> _cptr;
        while _cptr >@(s) _clim do
            _int() -> _cptr--!(s) -> _cptr
        endwhile
    else
        Get_string(_len) -> string;
        string@V_BYTES[_0]  -> _clim;
        _clim@(b)[_len] -> _cptr;
        while _cptr >@(b) _clim do
            _int() -> _cptr--!(b) -> _cptr
        endwhile
    endif
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 28 1997
        Made updater of pop_sys_encoding assign encoding pointer or NULL
        to _extern _pop_sys_encoding_funcs.
--- John Gibson, Mar 11 1997
        Added sys_decode_string and sys_encode_string
--- John Gibson, Mar  7 1997
        Added pop_sys_encoding, and Consstring_bptr to replace both
        Copy*_sysstring and Get*_nt_string.
--- John Gibson, Feb  4 1997
        String16 changes
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, May  6 1994
        Added Get_nt_string
--- John Gibson, Sep  2 1992
        Added M_K_NO_FULL_FROM_PTR to key flags
--- Robert John Duncan, Aug  7 1992
        Changed Endstring_to_num to return a system integer
--- John Gibson, Mar 15 1992
        Changed chars to be 24-bit, i.e. 0 - 16:FFFFFF (but with middle
        byte zero).
--- John Gibson, Jan 18 1992
        Made string procedures accept chars (i.e. ints upto 16:FFFF)
--- John Gibson, Jan  2 1992
        Added M_K_STRING flag to key and changed -isstring- to test
        for it rather than string_key explicitly
--- John Gibson, Dec 31 1991
        Changes to allow dstrings for string ops
--- John Gibson, Sep 14 1991
        Changes for new format bytevecs -- when an exact multiple of
        WORD_BYTES, they have an extra 0 word to guarantee null termination.
        Removed N*ull_end.
--- John Gibson, Apr  1 1991
        Added -Endstring_to_num-
--- John Gibson, Jan 24 1991
        -N*ull_end- moved in from sysutil.p
--- John Gibson, Apr  2 1990
        Changed K_SPEC to "byte"
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Dec  1 1989
        Changes for new pop pointers
--- John Gibson, Jul 31 1989
        Made -substring- return -nullstring- for an enpty string
--- John Gibson, Jan 25 1989
        Moved some things to end of file so it compiles with new version
        of popc (see comment above).
--- John Gibson, Feb 22 1988
        Created this file from string stuff in vectors.p, etc
 */
