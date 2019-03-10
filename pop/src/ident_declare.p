/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/ident_declare.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *IDENT
 */

;;; ----------------- DECLARING PERMANENT IDENTIFIERS ----------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'vmdefs.ph'

global constant
        procedure (intof, isreal, strnumber)
    ;

section $-Sys;

constant
        procedure (Check_token, Cons_perm_id, Assign_perm_id, Make_perm_def,
        Make_idval_pdr, Make_idval_active)
    ;

endsection;


;;; -----------------------------------------------------------------------

section $-Sys => ident_declare;

    ;;; Return ID_IDENTPROPS flags and ID_NUM_ATTRIBUTE value from idprops
define Idprops_flags(token, idprops, _const);
    lvars idprops, prec, token, _num = _0, _active = _0, _const;

    define lconstant is_op_prec(prec);
        lvars prec;
        if isreal(prec) then
            intof(prec*10) -> prec;
            if isinteger(prec) and prec /== 0
            and -128 fi_<= prec and prec fi_<= 127 then
                prec fi_<< 1 -> prec;
                return(if prec fi_< 0 then 0 fi_- prec else prec fi_+ 1 endif)
            endif
        endif;
        false
    enddefine;

    if ispair(idprops) then
        ;;; active
        fast_destpair(idprops) -> _num -> idprops;
        unless isinteger(_num)
        and 0 fi_<= _num and _num fi_<= 255 and (idprops == 0 or _num == 1)
        then
            mishap(token, _num, 2, 'ILLEGAL VALUE FOR ACTIVE MULTIPLICITY')
        endunless;
        _int(_num) -> _num;
        _:M_ID_ACTIVE -> _active
    endif;

    if idprops == 0 then
        _0
    elseif idprops == "procedure" then
        _:M_ID_PROCEDURE
    elseif idprops == "syntax" then
        _:M_ID_SYNTAX
    elseif idprops == "macro" then
        _:M_ID_MACRO
    elseif isword(idprops)
    and isstartstring('syntax\s', idprops)
    and (is_op_prec(strnumber(allbutfirst(7, idprops))) ->> prec) then
        ;;; syntax operator
        _int(prec) -> _num;
        _:M_ID_SYNTAX _biset _:M_ID_OPERATOR _biset _:M_ID_PROCEDURE
    elseif is_op_prec(idprops) ->> prec then
        ;;; operator
        _int(prec) -> _num;
        _:M_ID_OPERATOR _biset _:M_ID_PROCEDURE
    else
        mishap(token, idprops, 2, 'ILLEGAL VALUE FOR IDENTPROPS')
    endif;
    if _const then () _biset _:M_ID_CONSTANT endif _biset _active, _num
enddefine;


define ident_declare(token, idprops, _const);
    lvars   id, token, idprops, vm_flags = pop_vm_flags, _idprops, _nidprops,
            _num, _const, _weak = false, _was_weak = false;

    define lconstant Copy_id(id) -> id;
        lvars id;
        copy(id) -> id;
        id!ID_PERM_FLAGS _biclear _:M_PERM_SYS_REFER -> id!ID_PERM_FLAGS
    enddefine;

    Check_token(token);
    if isinteger(_const) then
        _int(_const) -> _const;
        _const _bitst _2:10 -> _weak;
        _const _bitst _2:01 -> _const
    endif;

    ;;; When running POPC, make all declarations weak
    if vm_pas_mode == "popc" and not(_weak) then 1 -> _weak endif;

    ;;; get new identprops flags, etc
    Idprops_flags(token, idprops, _const) -> (_nidprops, _num);

    if issimple(token!W_IDENTIFIER ->> id) then
        ;;; not declared
        Cons_perm_id(token) -> id;
        if _weak then _:M_PERM_NOT_DEF -> id!ID_PERM_FLAGS endif;
        _0 -> _idprops

    else
        ;;; already declared
        if id!ID_PERM_FLAGS _bitst _:M_PERM_NOT_DEF then
            ;;; only weakly declared
            true -> _was_weak;
            unless _weak then Make_perm_def(token) endunless
        elseif _weak and vm_pas_mode /== "popc" then
            ;;; ignore weak declaration if already strong
            return
        endif;

        if (id!ID_IDENTPROPS ->> _idprops) _bitst _:M_ID_PROTECT then
            ;;; protected system identifier
            unless vm_flags &&/=_0 VM_NOPROT_PVARS or _weak == true then
                ;;; yukky error for Lisp or Prolog!
                mishap(token, 1, 'idw: ILLEGAL DECLARATION OF WORD (missing ; after vars?)',
                                    'vm-ident-perm:name-decl-prot')
            elseif _idprops _bitst _:M_ID_ASSIGNED_CONST
            and id <@(w) _system_end then
                ;;; system constant -- write protected
                ;;; if doing POPC and id directly referenced in system
                ;;; then don't want to copy it, so do nothing
                if vm_pas_mode == "popc"
                and id!ID_PERM_FLAGS _bitst _:M_PERM_SYS_REFER then
                    ;;; ensure name attached to identifier
                    Assign_perm_id(token, id);
                    return
                else
                    ;;; have to copy id
                    Copy_id(id) -> id
                endif
            endunless
        endif;

        if vm_pas_mode == "popc" then
            if _nidprops _bitst _:M_ID_ACTIVE
            and not(_idprops _bitst _:M_ID_ACTIVE) then
                if id!ID_PERM_FLAGS _bitst _:M_PERM_SYS_REFER then
                    Copy_id(id) -> id
                endif;
                fast_idval(Cons_perm_id(token)) -> fast_idval(id)
            endif
        elseif pop_debugging or vm_flags &&=_0 VM_PERM_FIXED_DECLARE
        or vm_flags &&/=_0 VM_NOPROT_PVARS then
            ;;; just check activeness consistency
            if (_idprops _biset _nidprops) _bitst _:M_ID_ACTIVE then
                unless (_idprops _bimask _nidprops) _bitst _:M_ID_ACTIVE
                and id!ID_NUM_ATTRIBUTE == _num then
                    mishap(token, 1,
                        'REDECLARING ACTIVE/NONACTIVE STATUS OF IDENTIFIER',
                                        'vm-ident-perm-act:name-decl-ambig')
                endunless
            endif;
            ;;; if was procedure var, keep it like that
            (_idprops _bimask _:M_ID_PROCEDURE) _biset _nidprops -> _nidprops
        else
            ;;; check same
            if _was_weak then
                ;;; don't bother about const/vars agreement
                if _nidprops _bitst _:M_ID_CONSTANT then
                    _idprops _biset _:M_ID_CONSTANT
                else
                    _idprops _biclear _:M_ID_CONSTANT
                endif -> _idprops
            endif;
            if _idprops _biclear (_:M_ID_PROTECT _biset _:M_ID_ASSIGNED_CONST)
                == _nidprops
            and id!ID_NUM_ATTRIBUTE == _num then
                _idprops -> _nidprops
            else
                mishap(token, 1, 'REDECLARING PERMANENT IDENTIFIER STATUS',
                                            'vm-ident-perm:name-decl-ambig')
            endif
        endif
    endif;

    _nidprops -> id!ID_IDENTPROPS;
    _num -> id!ID_NUM_ATTRIBUTE;

    if _nidprops _bitst _:M_ID_PROCEDURE and not(isprocedure(fast_idval(id)))
    then
        Make_idval_pdr(id)
    endif;
    if _nidprops _bitst _:M_ID_ACTIVE and not(_idprops _bitst _:M_ID_ACTIVE)
    then
        Make_idval_active(id)
    endif;

    Assign_perm_id(token, id)
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  9 1996
        Added some mishap idstrings
--- John Gibson, Mar  5 1996
        Removed disabling of constants when pop_debugging == true
        (instead, non-system constants are now treated as vars by the VM).
--- John Gibson, Aug 16 1993
        Test for Popc now vm_pas_mode == "popc"
--- John Gibson, Oct  7 1992
        Made ident_declare coerce all declarations to weak when running POPC
--- Simon Nichols, Nov  8 1990
        Changed mishap codes to lower case.
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Jun  3 1989
        Replaced -popconstants- with pop_debugging /== true
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, May  2 1989
        Turning off protection on perm identifiers now done with
        VM_NOPROT_PVARS in -pop_vm_flags-.
--- John Gibson, Jan 29 1989
        Changes for 'weak' declarations
--- John Gibson, Mar 13 1988
        Stuff moved from ident.p
 */
