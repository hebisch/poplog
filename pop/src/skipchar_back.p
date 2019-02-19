/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/skipchar_back.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *STRINGS
 */

;;; ------- SKIP A CHARACTER IN A STRING SEARCHING BACKWARDS ---------------

#_INCLUDE 'declare.ph'

;;; ---------------------------------------------------------------------

section $-Sys => skipchar_back;

define skipchar_back(_char, _s, string);
    lvars string, _s, _char, _lim;
    Checkr_dchar(_char) ->;
    if isword(string) then
        string!W_STRING -> string
    else
        Check_bytevec(string)
    endif;
    Check_integer(_s, 0);
    _int(_s) -> _s;
    if _s _gr string!V_LENGTH then string!V_LENGTH -> _s endif;
    _int(_char) _bimask _16:FFFF -> _char;

    if string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        string@V_SHORTS[_0] -> _lim;
        _lim@(s)[_s] -> _s;
        while _s >@(s) _lim do
            if (_s--!(s) -> _s) /== _char then
                return(_pint( ##(s){_s, _lim} _add _1 ))
            endif
        endwhile
    else
        string@V_BYTES[_0] -> _lim;
        _lim@(b)[_s] -> _s;
        while _s >@(b) _lim do
            if (_s--!(b) -> _s) /== _char then
                return(_pint( ##(b){_s, _lim} _add _1 ))
            endif
        endwhile
    endif;
    false
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb  5 1997
        String16 changes
--- John Gibson, Mar 31 1988
        Moved out of vectors.p
 */
