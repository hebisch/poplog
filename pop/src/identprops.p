/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/src/identprops.p
 > Purpose:
 > Author:          John Gibson, Feb 28 1988 (see revisions)
 > Documentation:   REF *IDENT
 */

;;; ----------------- IDENTPROPS TESTING ETC -------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'

constant
        procedure Sys$-Float_op_prec;
    ;

;;; ----------------------------------------------------------------------

section $-Sys => identprops identtype isdeclared isdefined isglobal isactive
                 isassignable isconstant isprotected;

define lconstant Checkr_ident(item);
    lvars item, _key;
    if iscompound(item) then
        if (item!KEY ->> _key) == ident_key then
            ;;; is an identifier
            return(item)
        elseif _key!K_FLAGS _bitst _:M_K_ID_TOKEN then
            ;;; is a token
            return(item!W_IDENTIFIER)
        endif
    endif;
    mishap(item, 1, 'IDENTIFIER OR WORD NEEDED')
enddefine;

define lconstant Checkr_na_ident(item);
    lvars item, _active = _:M_ID_ACTIVE;
    if ispair(item) and fast_back(item) == "nonactive" then
        _0 -> _active;
        fast_front(item) -> item
    endif;
    Checkr_ident(item), _active
enddefine;

define Special_idprops(keyword, num);
    lvars keyword, num;
    dlocal pr = sys_syspr, pop_pr_quotes = false, pop_pr_radix = 10,
        weakref pop_pr_places = 1, weakref pop_pr_exponent = false
        ;
    ;;; syntax operator or active - make sure >< produces ordinary result
    consword(keyword >< '\s' >< num)
enddefine;

define identprops() with_nargs 1;
    lvars id = Checkr_ident(), prec, _idprops, _prec;
    if issimple(id) then
        "undef"
    elseif (id!ID_IDENTPROPS ->> _idprops) _bitst _:M_ID_OPERATOR then
        id!ID_NUM_ATTRIBUTE -> _prec;
        _shift(_prec, _-1),
            unless _prec _bitst _1 then _negate() endunless -> _prec;
        unless _zero(_pint(_prec _div _10) -> prec) then
            ;;; non-integral precedence
            if testdef decimal_key then
                ;;; OK to produce float idprops
                weakref[decimal_key] Float_op_prec(_prec) -> prec
            else
                mishap(0, 'CAN\'T PRODUCE IDENTPROPS (floating-point not loaded)')
            endif
        endunless;
        if _idprops _bitst _:M_ID_SYNTAX then
            Special_idprops("syntax", prec)
        else
            prec
        endif
    elseif _idprops _bitst _:M_ID_MACRO then
        "macro"
    elseif _idprops _bitst _:M_ID_SYNTAX then
        "syntax"
    else
        0
    endif
enddefine;

define identtype() with_nargs 1;
    lvars id = Checkr_ident(), _idprops;
    if issimple(id) then
        "undef"
    elseif (id!ID_IDENTPROPS ->> _idprops) _bitst _:M_ID_LEX_TOKEN
    and id!ID_LEX_FLAGS _bitst _:M_LEX_RTID_ACCESS then
        ;;; access is thru a run-time created identifier
        copy(id) ->> id;            ;;; return the copy
        id!ID_LEX_FLAGS _biclear _:M_LEX_RTID_ACCESS -> id!ID_LEX_FLAGS
    elseif _idprops _bitst _:M_ID_PROCEDURE then
        "procedure"
    else
        0
    endif
enddefine;

define isdeclared() with_nargs 1;
    lvars id = Checkr_ident();
    if iscompound(id) then id else false endif
enddefine;

define isdefined() with_nargs 1;
    lvars id = Checkr_ident();
    if iscompound(id)
    and (id!ID_IDENTPROPS _bitst _:M_ID_LEX
         or not(id!ID_PERM_FLAGS _bitst _:M_PERM_NOT_DEF) ) then
        id
    else
        false
    endif
enddefine;

define isglobal() with_nargs 1;
    lvars id = Checkr_ident();
    if issimple(id) or id!ID_IDENTPROPS _bitst _:M_ID_LEX then
        false
    else
        id!ID_PERM_FLAGS _bitst _:M_PERM_GLOBAL
    endif
enddefine;

define isactive() with_nargs 1;
    lvars id = Checkr_ident();
    if iscompound(id) and id!ID_IDENTPROPS _bitst _:M_ID_ACTIVE then
        if id!ID_IDENTPROPS _bitst _:M_ID_OPERATOR then
            1
        else
            _pint(id!ID_NUM_ATTRIBUTE)
        endif
    else
        false
    endif
enddefine;

define isassignable() with_nargs 1;
    lvars (id, _active) = Checkr_na_ident();
    if issimple(id) then
        true
    elseif id!ID_IDENTPROPS _bitst _active then
        fast_idval(id)!PD_UPDATER and true
    else
        not(id!ID_IDENTPROPS _bitst (_:M_ID_PROTECT _biset _:M_ID_ASSIGNED_CONST))
    endif
enddefine;

define isconstant() with_nargs 1;
    lvars (id, _active) = Checkr_na_ident(), _idprops;
    if issimple(id) then
        false
    elseif (id!ID_IDENTPROPS ->> _idprops) _bitst _active then
        not(fast_idval(id)!PD_UPDATER)
    elseif _idprops _bitst _:M_ID_ASSIGNED_CONST then
        true
    elseif _idprops _bitst _:M_ID_CONSTANT then
        "undef"
    else
        false
    endif
enddefine;

define isprotected() with_nargs 1;
    lvars (id, _active) = Checkr_na_ident();
    if issimple(id) then
        false
    elseif id!ID_IDENTPROPS _bitst _active then
        not(fast_idval(id)!PD_UPDATER)
    else
        id!ID_IDENTPROPS _bitst _:M_ID_PROTECT
    endif
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 16 1994
        Changed isassignable, isconstant and isprotected to test an active
        variable for having an updater.
--- John Gibson, Mar  5 1990
        Removed references to short_i*dent_key (no longer used).
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, Jan 29 1989
        New procedures -isdeclared-, -isdefined-
--- John Gibson, Feb 28 1988
        Created this file from stuff in ident.p
 */
