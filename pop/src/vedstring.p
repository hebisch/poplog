/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/vedstring.p
 > Purpose:         Procedures for manipulating Ved strings
 > Author:          John Gibson, Nov  7 1995 (see revisions)
 */

;;;------------------ VED STRINGS WITH EMBEDDED DATA ----------------------

#_INCLUDE 'declare.ph'

constant
        procedure (consdstring, fast_subscrdstring, subdstring,
        set_subvector, Sys$-User_underflow, Sys$-Fill_dstring)
    ;

;;; -----------------------------------------------------------------------

section $-Sys => consvedstring, fast_subscrvedstring, subvedstring;

lvars
    last_subscr_string = false,
    last_subscr_dvec,
;


define lconstant Grab_chardata(_n) -> _count;
    lvars vchar, _uptr, _ulim, _count = 0, _n = _int(_n);
    if _stklength() _lt @@(w)[_n] then User_underflow() endif;
    _user_sp() -> _ulim;
    _ulim@(w)[_n] -> _uptr;
    while _uptr >@(w) _ulim do
        _uptr--!(w) -> (vchar, _uptr);
        nextunless(ispair(vchar));
        fast_front(vchar) -> _uptr!(w);     ;;; replace with char
        _pint(_n _sub ##(w){_uptr, _ulim}); ;;; stack index of char in string
        fast_back(vchar);                   ;;; stack associated data
        _count fi_+ 2 -> _count
    endwhile
enddefine;

define consvedstring(_n) -> string;
    lvars string, dvec, _i, _n;
    Check_integer(_n, 0);
    returnif(_n == 0) (nullstring -> string);
    Grab_chardata(_n) -> _i;
    _i /== 0 and consvector((), _i) -> dvec;
    consdstring((), _n) -> string;
    if dvec then dvec -> vedstring_data_prop(string) endif
enddefine;

define fast_subscrvedstring(_ssub, string) -> vchar;
    lvars   string, dvec, vchar, _ptr, _lim, _i, _ssub;

    _int(_ssub) _sub _1 -> _i;
    string!KEY!K_FLAGS -> _ptr;
    if _ptr _bitst _:M_K_STRING16 then
        string!V_SHORTS[_i] -> _lim;
        if _ptr _bitst _:M_K_DSTRING then
            _shift(_DSTRING16_ATTR_CHAR(string, _i), _16) _add _lim -> _lim
        endif
    else
        string!V_BYTES[_i] -> _lim;
        if _ptr _bitst _:M_K_DSTRING then
            _shift(_DSTRING_ATTR_CHAR(string, _i), _16) _add _lim -> _lim
        endif
    endif;
    _pint(_lim) -> vchar;

    if last_subscr_string == string then
        last_subscr_dvec -> dvec
    else
        vedstring_data_prop(string) ->> dvec -> last_subscr_dvec;
        string -> last_subscr_string
    endif;
    returnunless(dvec);

    dvec@V_WORDS -> _ptr;
    _ptr@(w)[dvec!V_LENGTH] -> _lim;
    while _ptr <@(w) _lim do
        _ptr!(w)++ -> (_i, _ptr);
        if _i fi_>= _ssub then
            if _i == _ssub then conspair(vchar, _ptr!(w)) -> vchar endif;
            return
        else
            _ptr@(w)++ -> _ptr
        endif
    endwhile
enddefine;
;;;
define updaterof fast_subscrvedstring(vchar, _ssub, string);
    lvars string, dvec, vchar, data, _ptr, _lim, _i, _ssub;

    unless vedstring_data_prop(string) ->> dvec then
        if ispair(vchar) then
            fast_front(vchar) -> fast_subscrdstring(_ssub, string);
            consvector(_ssub, fast_back(vchar), 2) -> vedstring_data_prop(string);
            if last_subscr_string == string then
                false -> last_subscr_string
            endif
        else
            vchar -> fast_subscrdstring(_ssub, string)
        endif;
        return
    endunless;

    if ispair(vchar) then fast_front(vchar) else vchar endif
                        -> fast_subscrdstring(_ssub, string);

    dvec@V_WORDS -> _ptr;
    _ptr@(w)[dvec!V_LENGTH] -> _lim;
    #|  while _ptr <@(w) _lim and _ptr!(w) fi_< _ssub do
            (_ptr!(w)++)!(w)++ -> _ptr
        endwhile;
        if _ptr <@(w) _lim and _ptr!(w) == _ssub then
            _ptr@(w)[_2] -> _ptr
        endif;
        if ispair(vchar) then _ssub, fast_back(vchar) endif;
        while _ptr <@(w) _lim do
            (_ptr!(w)++)!(w)++ -> _ptr
        endwhile
    |# -> _i;
    if _pint(dvec!V_LENGTH) == _i then
        () -> explode(dvec)
    else
        _i /== 0 and consvector(_i) -> vedstring_data_prop(string);
        if last_subscr_string == string then false -> last_subscr_string endif
    endif
enddefine;

define subvedstring(_ssub, _len, string) -> substr;
    lvars   string, substr = subdstring(_ssub, _len, string), dvec, data,
            _ptr, _lim, _i, _ssub, _len;

    returnunless(vedstring_data_prop(string) ->> dvec);

    _ssub fi_+ _len -> _len;
    _ssub fi_- 1 -> _ssub;
    dvec@V_WORDS -> _ptr;
    _ptr@(w)[dvec!V_LENGTH] -> _lim;
    #|  while _ptr <@(w) _lim do
            _ptr!(w)++ -> (_i, _ptr);
            _ptr!(w)++ -> (data, _ptr);
            quitif(_i fi_>= _len);
            if _i fi_> _ssub then _i fi_- _ssub, data endif
        endwhile
    |# -> _i;
    if _i /== 0 then consvector(_i) -> vedstring_data_prop(substr) endif
enddefine;
;;;
define updaterof subvedstring(substr, _ssub, _len, string);
    lvars   string, substr, dvec, sdvec, data, _ptr, _lim, _sptr, _slim,
            _i, _ssub, _len;

    substr -> subdstring(_ssub, _len, string);

    vedstring_data_prop(substr) -> sdvec;
    vedstring_data_prop(string) -> dvec;
    returnunless(sdvec or dvec);

    dvec@V_WORDS -> _ptr;
    if dvec then _ptr@(w)[dvec!V_LENGTH] else _ptr endif -> _lim;

    sdvec@V_WORDS -> _sptr;
    if sdvec then _sptr@(w)[sdvec!V_LENGTH] else _sptr endif -> _slim;
    while _sptr <@(w) _slim and _slim!(w)[_-2] fi_> _len do
        _slim@(w)[_-2] -> _slim
    endwhile;

    _ssub fi_+ _len -> _len;
    _ssub fi_- 1 -> _ssub;

    #|  while _ptr <@(w) _lim and _ptr!(w) fi_<= _ssub do
            (_ptr!(w)++)!(w)++ -> _ptr
        endwhile;
        while _ptr <@(w) _lim and _ptr!(w) fi_< _len do
            _ptr@(w)[_2] -> _ptr
        endwhile;
        while _sptr <@(w) _slim do
            (_sptr!(w)++ -> _sptr) fi_+ _ssub;
            _sptr!(w)++ -> _sptr
        endwhile;
        while _ptr <@(w) _lim do
            (_ptr!(w)++)!(w)++ -> _ptr
        endwhile
    |# -> _i;

    if dvec and _pint(dvec!V_LENGTH) == _i then
        () -> explode(dvec)
    else
        _i /== 0 and consvector(_i) -> vedstring_data_prop(string);
        if last_subscr_string == string then false -> last_subscr_string endif
    endif
enddefine;

define Explode_subvedstring(/*_ssub, string,*/ _len);
    lvars string, _ssub, _len;
    if isstring(_len) then
        ;;; whole string
        1, _len, _pint(_len!V_LENGTH) -> (_ssub, string, _len)
    else
        () -> (_ssub, string)
    endif;
    _ssub fi_+ _len -> _len;
    while _ssub fi_< _len do
        _CHECKUSER;
        fast_subscrvedstring(_ssub, string);
        _ssub fi_+ 1 -> _ssub
    endwhile
enddefine;

define Fill_vedstring(string, _do_check);
    lvars string, dvec, _i, _n = _pint(string!V_LENGTH), _do_check;
    Grab_chardata(_n) -> _i;
    vedstring_data_prop(string) -> dvec;
    if dvec and _pint(dvec!V_LENGTH) == _i then
        () -> explode(dvec)
    elseif _i /== 0 or dvec then
        if last_subscr_string == string then false -> last_subscr_string endif
    endif;
    chain(string, _do_check, Fill_dstring)
enddefine;

define Copy_vedstring(string) -> new;
    lvars string, new, dvec;
    copy(string) -> new;
    if vedstring_data_prop(string) ->> dvec then
        copy(dvec) -> vedstring_data_prop(new)
    endif
enddefine;


    /*  _char is false if substring is being shifted out
    */
define Set_subvedstring(_char, _col, string, _ncols);
    lvars   string, dvec, data, _ptr, _lim, _i, _col, _ncols, _lcol, _n,
            _char;
    if _char then set_subvector(_char, _col, string, _ncols) endif;
    returnunless(vedstring_data_prop(string) ->> dvec);

    dvec@V_WORDS -> _ptr;
    _ptr@(w)[dvec!V_LENGTH] -> _lim;
    _col fi_+ _ncols -> _lcol;
    0 -> _n;
    #|  while _ptr <@(w) _lim do
            _ptr!(w)++ -> (_i, _ptr);
            _ptr!(w)++ -> (data, _ptr);
            if _i fi_>= _lcol then
                unless _char then _i fi_- _ncols -> _i endunless
            elseif _i fi_>= _col then
                nextloop
            endif;
            _i, data;
            _n fi_+ 2 -> _n
        endwhile
    |# -> _i;
    if _pint(dvec!V_LENGTH) == _i then
        () -> explode(dvec)
    else
        _i /== 0 and consvector(_i) -> vedstring_data_prop(string);
        if last_subscr_string == string then false -> last_subscr_string endif
    endif
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb  5 1997
        String16 changes
 */
