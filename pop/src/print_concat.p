/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/print_concat.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *PRINT
 */

;;;---------- CONCATENATE PRINTED REPRESENTATIONS OF ITEMS -----------------

#_INCLUDE 'declare.ph'

constant
        procedure (Sys$-Get_string, Sys$-Get_string16)
    ;

weak constant
        procedure (Sys$-Get_dstring, Sys$-Get_dstring16)
    ;


;;; ----------------------------------------------------------------------

section $-Sys => ><, sys_>< ;

define 5 item1 >< y;
    lvars item1, y, _savelen;
    dlocal cucharout = identfn;
    stacklength() -> _savelen;
    pr(item1);
    pr(y);
    consstring(stacklength() fi_- _savelen)
enddefine;

    /*  Concatenate any 2 strings
    */
define <>_strings(str1, str2) -> string;
    lvars   str1, str2, string, _rptr, _bptr, _flags, _flags1, _flags2,
            _len1 = str1!V_LENGTH, _len2 = str2!V_LENGTH,
            _lim, _len, _f1, _f2,
            ;

    returnif(_zero(_len1)) (str2 -> string);
    returnif(_zero(_len2)) (str1 -> string);
    _len1 _add _len2 -> _len;

    str1!KEY!K_FLAGS -> _flags1;
    str2!KEY!K_FLAGS -> _flags2;
    _flags1 _biset _flags2 -> _flags;

    if _flags _bitst _:M_K_STRING16 then
        _flags1 -> _f1, _flags2 -> _f2;
        if _flags _bitst _:M_K_DSTRING then
            weakref[dstring_key] Get_dstring16(_len)
        else
            Get_string16(_len)
        endif
    else
        _:M_K_STRING16 ->> _f1 -> _f2;
        if _flags _bitst _:M_K_DSTRING then
            weakref[dstring_key] Get_dstring(_len)
        else
            Get_string(_len)
        endif
    endif -> string;

    ;;; move in chars of first
    unless _f1 _bitst _:M_K_STRING16 then
        ;;; first string is 8, result 16
        string@V_SHORTS[_0] -> _rptr;
        str1@V_BYTES[_0] -> _bptr;
        _bptr@(b)[_len1] -> _lim;
        while _bptr <@(b) _lim do
            (_bptr!(b)++ -> _bptr) -> _rptr!(s)++ -> _rptr
        endwhile
    elseif _flags _bitst _:M_K_STRING16 then
        ;;; result 16
        _moveq(@@(w)[_len1|s.r], str1@V_WORDS, string@V_WORDS) -> ;
        string@V_SHORTS[_len1] -> _rptr
    else
        ;;; result 8
        _moveq(@@(w)[_len1|b.r], str1@V_WORDS, string@V_WORDS) -> ;
        string@V_BYTES[_len1] -> _rptr
    endunless;

    ;;; now move in fields of second
    unless _f2 _bitst _:M_K_STRING16 then
        ;;; second string is 8, result 16
        str2@V_BYTES[_0] -> _bptr;
        _bptr@(b)[_len2] -> _lim;
        while _bptr <@(b) _lim do
            (_bptr!(b)++ -> _bptr) -> _rptr!(s)++ -> _rptr
        endwhile
    elseif _flags _bitst _:M_K_STRING16 then
        ;;; result 16
        _smove(@@(s)[_len2], str2@V_SHORTS[_0], _rptr) ->
    else
        ;;; result 8
        _bmove(@@(b)[_len2], str2@V_BYTES[_0], _rptr) ->
    endunless;

    returnunless(_flags _bitst _:M_K_DSTRING);

    ;;; (d)string <> (d)string -> dstring
    if _flags _bitst _:M_K_STRING16 then
        string@(w){_STRING16_LIM_OFFS(_len)} -> _rptr
    else
        string@(w){_BYTEVEC_LIM_OFFS(_len)} -> _rptr
    endif;

    if _flags1 _bitst _:M_K_DSTRING then
        ;;; copy attributes from first
        _moveq(@@(w)[_len1|b.r],
                if _flags1 _bitst _:M_K_STRING16 then
                    str1@(w){_STRING16_LIM_OFFS(_len1)}
                else
                    str1@(w){_BYTEVEC_LIM_OFFS(_len1)}
                endif, _rptr) ->
    else
        ;;; zero attributes in first
        _fill(_0, @@(w)[_len1|b.r], _rptr)
    endif;

    _rptr@(w->b)[_len1] -> _rptr;
    if _flags2 _bitst _:M_K_DSTRING then
        ;;; copy attributes from second
        _bmove(@@(b)[_len2],
                if _flags2 _bitst _:M_K_STRING16 then
                    _DSTRING16_ATTR_PTR(str2, _0)
                else
                    _DSTRING_ATTR_PTR(str2, _0)
                endif, _rptr) ->
    else
        ;;; zero attributes in second
        _bfill(_0, @@(b)[_len2], _rptr)
    endif
enddefine;      /* <>_strings */

define 5 x sys_>< y;
    lvars   x, y;
    dlocal  cucharout, pop_pr_quotes, pop_pr_radix,
            weakref pop_pr_exponent, weakref pop_pr_places
        ;

    ;;; optimise joining of two strings or words
    if isstring(x) then
        returnif(isstring(y)) (<>_strings(x, y));
        if isword(y) then
            y!W_STRING -> y;
            returnif(x == nullstring) (copy(y));
            return(<>_strings(x, y))
        endif;
    elseif isword(x) then
        x!W_STRING -> x;
        if isstring(y) then
            returnif(y == nullstring) (copy(x));
            return(<>_strings(x, y))
        elseif isword(y) then
            y!W_STRING -> y;
            returnif(x == nullstring) (copy(y));
            returnif(y == nullstring) (copy(x));
            return(<>_strings(x, y))
        endif
    endif;

    ;;; else standard printing format
    6 -> weakref pop_pr_places;
    10 -> pop_pr_radix;
    false ->> pop_pr_quotes -> weakref pop_pr_exponent;
    identfn -> cucharout;
    consstring(#| sys_syspr(x), sys_syspr(y) |#)
enddefine;

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 12 1997
        String16 changes
--- John Gibson, Jul 14 1992
        Rewrote sys_>< so it never returns an actual word string
--- John Gibson, Apr 14 1988
        Moved out of old data.p
 */
