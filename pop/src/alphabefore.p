/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/alphabefore.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *STRINGS
 */

;;; ------- COMPARE TWO WORDS OR STRINGS FOR ALPHABETIC ORDERING --------

#_INCLUDE 'declare.ph'


;;; -------------------------------------------------------------------

section $-Sys => alphabefore;

define alphabefore(_s1, _s2);
    lvars c1, c2, _s1, _s2, _lim, _len, _len2, _res;
    if isword(_s1) then
        _s1!W_STRING -> _s1
    else
        unless isstring(_s1) then Check_string(_s1) endunless
    endif;
    if isword(_s2) then
        _s2!W_STRING -> _s2
    else
        unless isstring(_s2) then Check_string(_s2) endunless
    endif;
    returnif(_s1 == _s2) (1);

    _s1!V_LENGTH -> _len;
    _s2!V_LENGTH -> _len2;
    if _len == _len2 then
        1 -> _res
    elseif _len _lt _len2 then
        true -> _res
    else
        _len2 -> _len;
        false -> _res
    endif;

    if _s1!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        _s1@V_SHORTS -> _s1;
        _s1@(s)[_len] -> _lim;
        if _s2!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            _s2@V_SHORTS -> _s2;
            while _s1 <@(s) _lim do
                _s1!(s)++ -> (c1, _s1);
                _s2!(s)++ -> (c2, _s2);
                returnif(c1 /== c2) (c1 _lt c2)
            endwhile
        else
            _s2@V_BYTES -> _s2;
            while _s1 <@(s) _lim do
                _s1!(s)++ -> (c1, _s1);
                _s2!(b)++ -> (c2, _s2);
                returnif(c1 /== c2) (c1 _lt c2)
            endwhile
        endif
    else
        _s1@V_BYTES -> _s1;
        _s1@(b)[_len] -> _lim;
        if _s2!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            _s2@V_SHORTS -> _s2;
            while _s1 <@(b) _lim do
                _s1!(b)++ -> (c1, _s1);
                _s2!(s)++ -> (c2, _s2);
                returnif(c1 /== c2) (c1 _lt c2)
            endwhile
        else
            _s2@V_BYTES -> _s2;
            while _s1 <@(b) _lim do
                _s1!(b)++ -> (c1, _s1);
                _s2!(b)++ -> (c2, _s2);
                returnif(c1 /== c2) (c1 _lt c2)
            endwhile
        endif
    endif;
    _res
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 14 1997
        String16 changes
--- John Gibson, Oct  2 1992
        Got rid of use of _c*mpc_order
--- John Gibson, Apr  6 1988
        Moved out of wordutil.p
 */
