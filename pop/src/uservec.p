/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/src/uservec.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;------------------- USER VECTOR PROCEDURES -----------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'gctypes.ph'

global constant
        procedure (Sys$-Get_rawvec)
    ;


;;; -----------------------------------------------------------------------

section $-Sys;

    /*  Init a full vector
    */
define Init_uservec(_len, key) -> _vec;
    lvars key, _vec, _len, _offs, _lim;
    unless isinteger(_len) and _len fi_>= 0 then
        Check_integer(_len, 0)
    endunless;
    _int(_len) -> _len;
    @@(w)[_len] -> _offs;
    Get_store(@@V_WORDS{_offs} _sub @@POPBASE) -> _vec;
    key -> _vec!KEY;
    _len -> _vec!V_LENGTH;
    _fill(undef, _offs, _vec@V_WORDS)
enddefine;

    /*  Init a non-full vector
    */
define Init_usernfvec(_len, key);
    lvars key, _len;
    unless isinteger(_len) and _len fi_>= 0 then
        Check_integer(_len, 0)
    endunless;
    chain(_int(_len), key, true, Get_rawvec)
enddefine;

    /*  Cons a full vector
    */
define Cons_uservec(_len, key) -> _vec;
    lvars key, _vec, _len, _lim, _offs;
    unless isinteger(_len) and _len fi_>= 0 then
        Check_integer(_len, 0)
    endunless;
    _int(_len) -> _len;
    @@(w)[_len] -> _offs;
    Get_store(@@V_WORDS{_offs} _sub @@POPBASE) -> _vec;
    key -> _vec!KEY;
    _len -> _vec!V_LENGTH;
    _vec@V_WORDS -> _lim;
    _lim@(w){_offs} -> _len;
    ;;; essential that _vec is nonpop here in case we get stack underflow
    ;;; (in which case some elements won't have been initialised)
    while _len >@(w) _lim do
        -> _len--!(w) -> _len
    endwhile
enddefine;

    /*  Cons a non-full vector
    */
define Cons_usernfvec(_len, key) -> vec;
    lvars vec, key, _len;
    unless isinteger(_len) and _len fi_>= 0 then
        Check_integer(_len, 0)
    endunless;
    Get_rawvec(_int(_len), key, key!K_GC_TYPE == _:GCTYPE_USERVEC) -> vec;
    key!K_FAST_SUBSCR_V!PD_UPDATER -> key;
    until _len == 0 do
        fast_apply(_len, vec, key);
        _len fi_- 1 -> _len
    enduntil
enddefine;

    /*  Dest a full vector
    */
define Dest_uservec(vec, _key);
    lvars vec, _offs, _lim, _key;
    unless iscompound(vec) and vec!KEY == _key then
        Record_needed(vec, _key)
    endunless;

    @@V_WORDS[_0] -> _offs;
    @@V_WORDS[vec!V_LENGTH] -> _lim;
    while _offs _lt _lim do
        _CHECKUSER;
        vec!(w){_offs};
        @@(w){_offs}++ -> _offs
    endwhile;
    _pint(vec!V_LENGTH)
enddefine;

    /*  Dest a non-full vector
    */
define Dest_usernfvec(vec, _key);
    lvars vec, procedure acc_p, _pos, _limit, _key;
    unless iscompound(vec) and vec!KEY == _key then
        Record_needed(vec, _key)
    endunless;

    vec!KEY!K_FAST_SUBSCR_V -> acc_p;
    1 -> _pos;
    _pint(vec!V_LENGTH) -> _limit;
    until _pos fi_> _limit do
        _CHECKUSER;
        acc_p(_pos, vec);
        _pos fi_+ 1 -> _pos
    enduntil;
    _limit
enddefine;

    /*  Check-and-return vector and subscript
    */
define Checkr_vec_subscr(_subs, item, _key) -> item -> _subs;
    lvars item, _subs, _key;
    unless iscompound(item) and item!KEY == _key then
        Record_needed(item, _key)
    elseunless isinteger(_subs) and _int(_subs) _sub _1 _lt item!V_LENGTH then
        mishap(_subs, item, 2, 'BAD SUBSCRIPT FOR INDEXED ACCESS')
    endunless
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec  7 1989
        Changes for new pop pointers
--- John Gibson, Mar 17 1989
        Created from run-time procedures from conskey.p
 */
