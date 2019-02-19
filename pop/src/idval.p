/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/src/idval.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *IDENT
 */

;;; ------------ ACCESSING/UPDATING IDENTIFIER VALUES -------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE '../lib/include/vm_flags.ph'


;;; ---------------------------------------------------------------------

section $-Sys => idval, nonactive_idval;

lvars
    _no_prot = false,
    ;;; set to a word when using Nonactive_assign in the VM
    _vm_idval_token = false,
;

lconstant
    id_needed = 'IDENTIFIER NEEDED';

define Checkr_upd_idval(/* newval, */ id, _token) with_nargs 3;
    lvars id, _idprops = id!ID_IDENTPROPS, _token, _errms;
    if _idprops _bitst _:M_ID_PROTECT and not(_no_prot) then
        'ILLEGAL ASSIGNMENT TO PROTECTED IDENTIFIER'
    elseif _idprops _bitst _:M_ID_ASSIGNED_CONST then
        'ILLEGAL ASSIGNMENT TO CONSTANT IDENTIFIER'
    elseif _idprops _bitst _:M_ID_PROCEDURE_VAL and not(isprocedure(dup()))
    then
        'ASSIGNING NON-PROCEDURE TO PROCEDURE IDENTIFIER'
    else
        if _idprops _bitst _:M_ID_CONSTANT
        and (_idprops _bitst _:M_ID_LEX
             or not(id!ID_PERM_FLAGS _bitst _:M_PERM_NOT_DEF))
        then
            _idprops _biset _:M_ID_ASSIGNED_CONST -> id!ID_IDENTPROPS
        endif;
        return
    endif -> _errms;
    unless _token then _vm_idval_token or id -> _token endunless;
    mishap((), _token, 2, _errms)
enddefine;

define idval(id);
    lvars id, _idprops;
    if iscompound(id) and id!KEY == ident_key then
        if (id!ID_IDENTPROPS ->> _idprops) _bitst _:M_ID_ACTIVE
        and not(_idprops _bitst _:M_ID_LEX_TOKEN) then
            fast_apply(fast_idval(id))
        else
            fast_idval(id)
        endif
    else
        mishap(id, 1, id_needed)
    endif
enddefine;
;;;
define updaterof idval(/* newval, */ id) with_nargs 2;
    lvars id, _idprops;
    if iscompound(id) and id!KEY == ident_key then
        if (id!ID_IDENTPROPS ->> _idprops) _bitst _:M_ID_ASSIGN_CHECK
        and not(_idprops _bitst _:M_ID_LEX_TOKEN) then
            if _idprops _bitst _:M_ID_ACTIVE then
                false -> _no_prot;
                () -> fast_apply(fast_idval(id));
                return
            else
                Checkr_upd_idval((), id, false)
            endif
        endif;
        () -> fast_idval(id)
    else
        mishap(id, 1, id_needed)
    endif
enddefine;

define nonactive_idval(id);
    lvars id;
    if iscompound(id) and id!KEY == ident_key then
        fast_idval(id)
    else
        mishap(id, 1, id_needed)
    endif
enddefine;
;;;
define updaterof nonactive_idval(/* newval, */ id) with_nargs 2;
    lvars id, _idprops;
    if iscompound(id) and id!KEY == ident_key then
        if (id!ID_IDENTPROPS ->> _idprops) _bitst _:M_ID_ASSIGN_CHECK
        and not(_idprops _bitst _:M_ID_LEX_TOKEN) then
            Checkr_upd_idval((), id, false)
        endif;
        () -> fast_idval(id)
    else
        mishap(id, 1, id_needed)
    endif
enddefine;

define Nonactive_assign(/*item, id,*/ _vm_idval_token);
    dlocal _vm_idval_token, _no_prot = pop_vm_flags &&/=_0 VM_NOPROT_PVARS;
    (/*item*/) -> nonactive_idval(/*id*/)
enddefine;

define Noprot_assign(/*item, id*/);
    dlocal _no_prot = true;
    (/*item*/) -> idval(/*id*/)
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 16 1994
        Added Noprot_assign
--- John Gibson, Nov  3 1992
         Changes to Checkr_upd_idval
--- John Gibson, Mar  5 1990
        Removed references to short_i*dent_key (no longer needed)
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, Apr  5 1988
        Moved out of ident.p
 */
