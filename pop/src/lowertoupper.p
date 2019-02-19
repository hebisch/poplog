/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/lowertoupper.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *STRINGS
 */

;;; ---------- TRANSLATE CHARACTERS, STRINGS ETC, TO LOWERCASE -------------

#_INCLUDE 'declare.ph'

global constant
        procedure islowercode
    ;

global vars
        pop_character_set
    ;

;;; ----------------------------------------------------------------------

section $-Sys => lowertoupper;

define lowertoupper(item);
    lvars item, string, _n, _c, _key;
    if issimple(item) then
        if islowercode(item) then item fi_- 16:20 else item endif
    elseif (item!KEY -> _key, _key!K_FLAGS _bitst _:M_K_STRING) then
        item -> string;
        string!V_LENGTH -> _n;
        _key!K_FLAGS _bitst _:M_K_STRING16 -> _key;
        until _zero(_n) do
            _n _sub _1 -> _n;
            if _key then
                string!V_SHORTS[_n] -> _c
            else
                string!V_BYTES[_n] -> _c
            endif;
            if _:`a` _lteq _c and _c _lteq _:`z`
            ;;; agrave <= c <= thorn and c /== divide
            or (_16:E0 _lteq _c and _c _lteq _16:FE and _c /== _16:F7
                and pop_character_set == `1`)   ;;; = Latin 1
            then
                if string == item then copy(string) -> string endif;
                _c _sub _16:20 -> _c;
                if _key then
                    _c -> string!V_SHORTS[_n]
                else
                    _c -> string!V_BYTES[_n]
                endif
            endif
        enduntil;
        string
    elseif _key == word_key then
        if (lowertoupper(item!W_STRING) ->> string) == item!W_STRING then
            ;;; unchanged
            item
        else
            Cons_word(string, true)     ;;; true = garbage string for existing word
        endif
    else
        item
    endif
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 14 1997
        String16 changes
--- John Gibson, Feb 24 1992
        Added ISO Latin 1 support
--- John Gibson, Jan 18 1992
        Modified to work with dchars
--- John Gibson, Mar 19 1989
        Improved handling of string and word cases
--- John Gibson, Mar 31 1988
        Moved out of util.p
 */
