/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/allbutlast.p
 > Purpose:
 > Author:          Aaron Sloman (see revisions)
 > Documentation:   REF *STRINGS
 */

;;; ------------ REMOVE LAST N ELEMENTS OF A STRUCTURE ------------------

#_INCLUDE 'declare.ph'

global constant
        procedure subword
    ;

weak global constant
        procedure subdstring
    ;

;;; ----------------------------------------------------------------------

section $-Sys => allbutlast;

    ;;; _n is a number, structure a list or string or vectorclass or word
define allbutlast(_n, structure);
    lvars key, structure, _len, _n, _flags;
    Check_integer(_n, 0);
    datakey(structure) -> key;
    length(structure) fi_- _n -> _len;  ;;; checks a list all the way down
    if _len fi_< 0 then
        mishap(_n, structure, 2, 'sts: STRUCTURE TOO SHORT')
    elseif (key!K_FLAGS ->> _flags) _bitst _:M_K_STRING then
        if _flags _bitst _:M_K_DSTRING then
            weakref[dstring_key] subdstring(1, _len, structure)
        else
            substring(1, _len, structure)
        endif
    elseif _flags _bitst _:M_K_VECTOR then
        fast_apply(structure, key!K_DEST_V) -> ;
        erasenum(_n);
        fast_apply(_len, key!K_CONS_V)
    elseif key == word_key then
        subword(1, _len, structure)
    elseif ispair(structure) or structure == [] then
        [%  until _len == 0 do
                _CHECKUSER;
                fast_destpair(structure) -> structure;
                _len fi_- 1 -> _len
            enduntil
        %]
    else
        mishap(structure, 1, 'INVALID ARGUMENT FOR allbutlast')
    endif
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 24 1997
        String changes
--- John Gibson, Jan 18 1992
        Deals with dstrings
--- Simon Nichols, Nov  8 1990
        Changed mishap codes to lower case.
--- John Gibson, Mar 31 1988
        Moved out of util.p
 */
