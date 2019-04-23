/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/ved/src/vdveditem.p
 > Purpose:
 > Author:          Aaron Sloman (see revisions)
 */

;;; --------- PROCEDURES FOR CHECKING VED ITEM BOUNDARIES -----------------

#_INCLUDE 'vddeclare.ph'

constant
        procedure (Sys$-Ved$-Col_is_inside_tab)
    ;


;;; -----------------------------------------------------------------------

section $-Sys$-Ved => vedchartype, vedatitemstart, vedatitemend, vedissubitem;

define vars vedchartype(char);
    lvars char;
    ;;; given a character return an integer code representing its type
    Checkr_dchar(char) fi_&& 16:FFFF -> char;
    if isalphacode(char) then
        `a`
    elseif char == `\s` then
        `\s`
    elseif locchar(char, 1, '()[]{}",;%.') then
        ;;; punctuation
        `.`
    elseif isnumbercode(char) then
        `0`
    elseif locchar(char, 1, '+-><*?=^!#$&~|\@:/') then
        ;;; sign
        `+`
    elseif locchar(char, 1, '\Ss\Sn\Sf\Sp\St') then
        ;;; special space
        `\s`
    else
        ;;; control character, `_`, `'`, ```
        char
    endif
enddefine;

    ;;; given a pointer to an integer occurring inside a string, is it part of
    ;;; an integer text item, or mixed-type identifier text item
define lconstant Inside_number(_x, string);
    lvars string, _x, _y, _type;
    if _x == 1 then
        ;;; if string starts with a number, this must be a number
        true
    else
        while (_x fi_- 1 ->> _x) /== 0 do
            fast_subscrs(_x, string) -> _y;
            vedchartype(_y) -> _type;
            if _type == `a` or (_y == `_`)
            then
                return(false)   ;;; alphanumeric type identifier
            elseif _type /== `0` then
                return(true)    ;;; found something that can't join up with numbers
                ;;; will not recognise decimal numbers at present
            endif
        endwhile;
        true
    endif
enddefine;

    ;;; true if _col-th character in string of length at most _limit is
    ;;; at the beginning of a POP11 text item
define vedatitemstart(_col, string, _limit);
    lvars string, _type, _last, _col, _limit;

    _col == 1 or _col fi_> _limit
    or (vedchartype(fast_subscrs(_col fi_- 1, string)) ->> _last) == `\s`
    or _last == `.`         ;;; needs to be changed for decimal numbers
    or (vedchartype(fast_subscrs(_col, string)) ->> _type) == `\s`
    or _type == `.`         ;;; needs to be changed for decimal numbers
    or (if _type == `\t` then
            return(not(Col_is_inside_tab(_col, string)))
        elseif _last == `\t` then
            return(true)
        elseif _type == _last
            or (_type == `0` and _last == `a`)
            or _last == `_`
            or (_type == `_`
                and not(_last == `0` and Inside_number(_col fi_- 1, string)))
        then
            return(false)
        else
            _last /== `0`
        endif)
    or _type /== `a`
    or Inside_number(_col fi_- 1, string)
enddefine;

    ;;; test whether _col-th character in string of length at most _limit
    ;;; is at at the end of a legitimate POP item
define vedatitemend(_col, string, _limit);
    lvars string, _type, _next, _col, _limit;
    if _col fi_>= _limit or _col == 0
        or vedatitemstart(_col fi_+ 1, string, _limit)
    then
        true
    else
        false
    endif
enddefine;

define vedissubitem(item, _loc, string) -> _loc;
    lvars item, string, _loc, _len, _lim;
    datalength(item) fi_- 1 -> _len;
    datalength(string) -> _lim;
    while issubstring(item,_loc,string) ->> _loc do
        if vedatitemstart(_loc,string,_lim)
        and vedatitemend(_loc fi_+ _len, string, _lim)
        then
            return
        else
            _loc fi_+ 1 -> _loc
        endif
    endwhile
enddefine;

endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 23 1993
        Made Ved special spaces be treated as spaces by vedchartype
--- John Gibson, Dec 11 1992
        i*ssubitem -> vedissubitem
--- John Gibson, Apr 21 1992
        Sectionised.
--- John Gibson, Feb 17 1992
        Fixed -vedatitemstart- to return false for an underscore preceded
        by numerals at the end of a word (e.g. pop11_ )
--- John Gibson, Feb 14 1988
        Procedures from util.p and vdsearch.p to create this file
 */
