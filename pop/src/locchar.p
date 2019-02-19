/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/locchar.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *STRINGS
 */

;;; --------------- LOCATE A CHARACTER IN A STRING -----------------------

#_INCLUDE 'declare.ph'

global constant
        _locc
    ;

;;; ---------------------------------------------------------------------

section $-Sys => locchar, strmember;

define locchar(_char, _s, string);
    lvars string, _s, _x, _char;
    Checkr_dchar(_char) -> ;
    if isword(string) then
        string!W_STRING -> string
    else
        Check_bytevec(string)
    endif;
    Check_integer(_s, 1);
    _int(_s) _sub _1 -> _s;
    returnif(_s _greq string!V_LENGTH) (false);
    _int(_char) _bimask _16:FFFF -> _char;

    if string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        string@V_SHORTS[_s] -> _s;
        string@V_SHORTS[string!V_LENGTH] -> _x;
        while _s <@(s) _x do
            returnif((_s!(s)++ -> _s) == _char)
                        (_pint(##(s){_s, string@V_SHORTS}))
        endwhile;
        false
    else
        returnif(_char _gr _16:FF) (false);
        _locc(string@V_BYTES[_s], @@(b)[string!V_LENGTH _sub _s], _char) -> _x;
        if _x == _-1 then
            false
        else
            _pint(##(b){_x} _add _s _add _1)
        endif
    endif
enddefine;

define strmember(char, string);
    lvars string, char;
    ;;; return false or location of first occurrence of character
    locchar(char, 1, string)
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb  5 1997
        String16 changes
--- John Gibson, Mar 31 1988
        Moved out of vectors.p
 */
