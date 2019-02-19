/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/valof.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *IDENT
 */

;;; ----------- ACCESSING PERM IDENTIFIERS ATTACHED TO WORDS -----------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'

constant
        procedure nonactive_idval
    ;

section $-Sys;

constant
        procedure (Check_token, Get_perm_ident, Assign_perm_id,
        Checkr_upd_idval)
    ;

endsection;

;;; ------------------------------------------------------------------------

section $-Sys => identof valof nonactive_valof recursive_valof;

define identof(/* token*/) with_nargs 1;
    Get_perm_ident((), false)
enddefine;
;;;
define updaterof identof(newid, token);
    lvars newid, token;
    Check_token(token);
    if iscompound(newid) and newid!KEY == ident_key
    and not(newid!ID_IDENTPROPS _bitst _:M_ID_LEX) then
        Assign_perm_id(token, newid)
    else
        mishap(newid, token, 2, 'PERMANENT IDENTIFIER NEEDED')
    endif
enddefine;

define valof(id);
    lvars id;
    unless isident(id) then Get_perm_ident(id, false) -> id endunless;
    if id!ID_IDENTPROPS _bitst _:M_ID_ACTIVE then
        fast_apply(fast_idval(id))
    else
        fast_idval(id)
    endif
enddefine;
;;;
define updaterof valof(/* newvalue, */ token) with_nargs 2;
    lvars id = token, token, _idprops;
    unless isident(id) then Get_perm_ident(id, false) -> id endunless;
    if (id!ID_IDENTPROPS ->> _idprops) _bitst _:M_ID_ASSIGN_CHECK then
        if _idprops _bitst _:M_ID_ACTIVE then
            () -> fast_apply(fast_idval(id));
            return
        else
            Checkr_upd_idval((), id, token)
        endif
    endif;
    () -> fast_idval(id)
enddefine;

define nonactive_valof(id);
    lvars id;
    unless isident(id) then Get_perm_ident(id, false) -> id endunless;
    fast_idval(id)
enddefine;
;;;
define updaterof nonactive_valof(/* newvalue, */ token) with_nargs 2;
    lvars id = token, token, _idprops;
    unless isident(id) then Get_perm_ident(id, false) -> id endunless;
    if (id!ID_IDENTPROPS ->> _idprops) _bitst _:M_ID_ASSIGN_CHECK
    and not(_idprops _bitst _:M_ID_LEX_TOKEN) then
        Checkr_upd_idval((), id, token)
    endif;
    () -> fast_idval(id)
enddefine;

define recursive_valof(token) -> token;
    lvars token, val;
    while isword(token) do
        _CHECKINTERRUPT;
        fast_idval(identof(token)) -> val;
        if token == val then
            mishap(token,1,'rvl: RECURSIVE_VALOF LOOPING')
        else
            val -> token
        endif
    endwhile
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, May 30 1995
        valof now works on identifiers as well as words (at last!)
--- John Gibson, Oct  6 1992
        Extra arg for Get_perm_ident
--- Simon Nichols, Nov  8 1990
        Changed mishap codes to lower case.
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, Jan 29 1989
        Changes for 'weak' declarations
--- John Gibson, Mar 14 1988
        Created from stuff in ident.p
 */
