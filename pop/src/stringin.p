/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/stringin.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *STRINGS
 */

;;; --------------- STRING CHARACTER REPEATER ----------------------------

#_INCLUDE 'declare.ph'

;;; ----------------------------------------------------------------------

section $-Sys => stringin;

define Stringin(statepair);
    lvars string, statepair, _position, _pos;
    fast_destpair(statepair) -> (string, _position);
    _int(_position) -> _pos;
    if _pos _gr string!V_LENGTH then
        termin
    else
        if string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            _pint(string!V_SHORTS[_pos _sub _1])
        else
            _pint(string!V_BYTES[_pos _sub _1])
        endif;
        _position fi_+ 1 -> fast_back(statepair)
    endif
enddefine;

define stringin(string);
    lvars string;
    if isword(string) then string!W_STRING -> string endif;
    Check_string(string);
    Stringin(% conspair(string, 1) %)
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 14 1997
        String16 changes
--- John Gibson, Mar 14 1988
        Moved out of vectors.p
 */
