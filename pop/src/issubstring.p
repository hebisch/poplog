/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/issubstring.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *STRINGS
 */

;;; ------------ TESTING FOR A SUBSTRING OF A STRING ---------------------

#_INCLUDE 'declare.ph'


;;; ---------------------------------------------------------------------

section $-Sys => issubstring_lim, issubstring, isendstring;


define issubstring_lim(substr, _index, _start_lim, _end_lim, string);
    lvars   p, subp, c, substr, string, _string, _sublim, _startlast, _substr,
            _start_lim, _end_lim, _sublen, _index, _len;
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

    Check_integer(_index, 1);
    _int(_index) _sub _1 -> _index;
    string!V_LENGTH -> _len;

    if _start_lim then
        Check_integer(_start_lim, 1);
        _int(_start_lim) _sub _1 -> _start_lim
    else
        _len -> _start_lim
    endif;

    ;;; set the actual limit of significant characters of string to _end_lim
    if _end_lim then
        Check_integer(_end_lim, 1);
        _int(_end_lim) -> _end_lim;
        if _end_lim _gr _len then _len -> _end_lim endif;
    else
        _len -> _end_lim
    endif;

    ;;; set limit defined by _start_lim and length of substring
    substr!V_LENGTH -> _sublen;
    _start_lim _add _sublen -> _start_lim;
    if _start_lim _lt _end_lim then _start_lim -> _end_lim endif;

    _end_lim _sub _index -> _end_lim;
    returnif(_neg(_end_lim)) (false);
    returnif(_zero(_sublen)) (_pint(_index _add _1));
    _end_lim _sub _sublen -> _end_lim;
    returnif(_neg(_end_lim)) (false);


    define :inline lconstant SUBSTR_CODE(ST, ST_V_FLD, T, T_V_FLD);
        string@T_V_FLD -> _start_lim;
        string@T_V_FLD[_index] -> _string;
        _string@(T)[_end_lim] -> _end_lim;
        substr@ST_V_FLD[_0] -> _substr;
        _substr@(ST)[_sublen] -> _sublim;
        _substr!(ST)++ -> (c, _substr);

        repeat
            if (_string!(T)++ -> _string) == c then
                _substr -> subp;
                _string -> p;
                repeat
                    returnif(subp >=@(ST) _sublim)
                                        ( _pint(##(T){_string,_start_lim}) );
                    quitunless((subp!(ST)++ -> subp) == (p!(T)++ -> p))
                endrepeat
            endif;
            quitunless(_string <=@(T) _end_lim)
        endrepeat
    enddefine;

    if string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        if substr!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            SUBSTR_CODE(s, V_SHORTS, s, V_SHORTS)
        else
            SUBSTR_CODE(b, V_BYTES,  s, V_SHORTS)
        endif
    else
        if substr!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            SUBSTR_CODE(s, V_SHORTS, b, V_BYTES)
        else
            SUBSTR_CODE(b, V_BYTES,  b, V_BYTES)
        endif
    endif;
    false
enddefine;


define issubstring(string) with_nargs 3;
    lvars string;
    unless isinteger(dup()) then 1 endunless;
    issubstring_lim((), (), false, false, string)
enddefine;


define isendstring(endstr, string);
    lvars endstr, string, _s = datalength(string) fi_- datalength(endstr);
    if _s fi_< 0 then
        false
    else
        issubstring_lim(endstr, _s fi_+ 1, false, false, string)
    endif
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 10 1997
        String16 changes (removed use of _mt*chc subroutine)
--- John Gibson, Dec 23 1990
        Changed 3rd and 4th args to call of -issubstring_lim- in
        -isendstring- to false.
--- John Williams, Dec  5 1988
        Added -isendstring- (replaces -Sys$-Ved$-isendstring-)
--- Aaron Sloman, May  6 1988
        Made the integer argument for -issubstring- optional
--- John Gibson, Apr  5 1988
        Moved out of vectors.p. Made -issubstring- just use
        -issubstring_lim-.
 */
