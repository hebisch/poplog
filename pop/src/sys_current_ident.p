/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/sys_current_ident.p
 > Purpose:         Get identifier associated with a word etc
 > Author:          John Gibson, May 20 1993 (see revisions)
 > Documentation:   REF * VMCODE
 */

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'vmdefs.ph'

constant
        procedure (nonactive_idval, Sys$-Check_token,
        Sys$-Note_perm_dependency, Sys$-Nonactive_assign)
;

vars
        procedure (sysdeclare)
;

weak constant
        procedure (Sys$-Vm$-Assign_pas_token, Sys$-Vm$-Passign_lconst)
;

weak vars
        pop_vm_dummy_idents
;


;;; ----------------------------------------------------------------------

section $-Sys$-Vm =>    sys_current_ident, sys_use_current_ident,
                        sys_current_val;

lconstant macro (
    POP_VM_DUMMY_IDENTS = [VM_WEAK pop_vm_dummy_idents],
    POP_SYNTAX_ONLY     = [VM_WEAK pop_syntax_only],
);

lvars dummy_lvar;   ;;; for returning dummy ident

    /*  Return any identifier currently associated with a word,
        or false if none.
    */
define sys_current_ident(word) -> id;
    lvars word, id, _idprops, _try_nofast, _tmp;

    if isboolean(word) then (), word -> (word, _try_nofast) endif;
    Check_token(word);

    unless VM_LOADED then
        ;;; VM not loaded
        if issimple(word!W_IDENTIFIER ->> id) then false -> id endif;
        return
    endunless;

    if ispair(POP_VM_DUMMY_IDENTS)
    and fast_lmember(word, POP_VM_DUMMY_IDENTS) then
        ;;; just generate a dummy identifier
        ident dummy_lvar -> id
    elseif VM_WEAK list_assoc_val(word, VM_WEAK vm_lex_assoc) ->> id then
        ;;; currently declared as a lexical identifier
        ;;; return the associated lex identifier
        id!ID_IDENTPROPS -> _idprops;
        if not(_idprops _bitst _:M_ID_LEX_TOKEN)
        or _idprops _bitst _:M_ID_CONSTANT then
            ;;; for a global or an lconstant, return the home id
            while id!ID_LEX_FLAGS _bitst _:M_LEX_NON_LOCAL do
                fast_idval(id) -> id
            endwhile
        endif
    else
        if issimple(word!W_IDENTIFIER ->> id) then
            ;;; not lexically or permanently declared
            false -> id
        else
            if _try_nofast and pop_vm_flags &&/=_0 VM_NO_FAST
            and (valof("pop_nofast_trans")(word, false) ->> _tmp) then
                _tmp -> word;
                word!W_IDENTIFIER -> id
            endif;
            if vm_pas_mode then VM_WEAK Assign_pas_token(id, word) endif
        endif
    endif
enddefine;

    /*  Return the identifier currently associated with a word and mark used,
        forcing the word to be declared.
    */
define sys_use_current_ident(word);
    lvars   word, id, depend_vec = false, _flags,
            _nonactive = false, _weakref = false, _weak_option = false,
            _try_nofast = false, _tmp;

    if isboolean(word) then (), word -> (word, _try_nofast) endif;

    while ispair(word) do
        fast_back(word) -> id;
        if isref(id) then
            ;;; tells POPC this shouldn't be counted as a weak reference
            true -> _weak_option;
            fast_cont(id) -> id
        endif;
        if id == "nonactive" then
            true -> _nonactive
        elseif id == "weakref" then
            true -> _weakref
        elseif isvector(id) then
            true -> _weakref;
            id -> depend_vec
        else
            mishap(word, 1, 'UNRECOGNISED VALUE IN sys_use_current_ident PAIR ARGUMENT')
        endif;
        fast_front(word) -> word
    endwhile;

    Check_token(word);

    if VM_LOADED then
        if ispair(POP_VM_DUMMY_IDENTS)
        and fast_lmember(word, POP_VM_DUMMY_IDENTS) then
            ;;; just generate a dummy identifier
            return(ident dummy_lvar, _nonactive)

        elseif VM_WEAK list_assoc_val(word, VM_WEAK vm_lex_assoc) ->> id then
            ;;; currently declared as a lexical identifier
            if _weakref and not(POP_SYNTAX_ONLY) then
                mishap(word, 1, 'INVALID WEAK REFERENCE TO LEXICAL IDENTIFIER')
            endif;

            ;;; return the associated lex identifier
            id;                     ;;; return the original id
            ;;; mark used all the way up the chain
            repeat
                id!ID_LEX_FLAGS -> _flags;
                quitif(_flags _bitst _:M_LEX_USED);
                _flags _biset _:M_LEX_USED -> id!ID_LEX_FLAGS;
                quitunless(_flags _bitst _:M_LEX_NON_LOCAL);
                fast_idval(id) -> id
            endrepeat;
            id!ID_IDENTPROPS -> _flags;
            if not(_flags _bitst _:M_ID_LEX_TOKEN)
            or _flags _bitst _:M_ID_CONSTANT then
                ;;; for a global or an lconstant, return the home id
                -> ;                            ;;; erase previous id
                ;;; follow the chain to the home identifier
                while id!ID_LEX_FLAGS _bitst _:M_LEX_NON_LOCAL do
                    fast_idval(id) -> id
                endwhile;
                id                              ;;; return the home id
            endif;
            return((), _nonactive);

        elseif vm_pas_mode == "popc" and not(POP_SYNTAX_ONLY) then
            valof("popc_auto_declare")(word)
        endif
    endif;

    if iscompound(word!W_IDENTIFIER ->> id)
    and (not(id!ID_PERM_FLAGS _bitst _:M_PERM_NOT_DEF)
         or _weakref or vm_pas_mode == "popc")
    then
        ;;; has an associated perm ident
        if VM_LOADED then
            if _try_nofast and pop_vm_flags &&/=_0 VM_NO_FAST
            and not(POP_SYNTAX_ONLY)
            and (valof("pop_nofast_trans")(word, false) ->> _tmp) then
                _tmp -> word;
                word!W_IDENTIFIER -> id
            endif;
            if vm_pas_mode then VM_WEAK Assign_pas_token(id, word) endif;
            returnif(POP_SYNTAX_ONLY) (id, _nonactive)
        endif;

        if _weakref and vm_pas_mode == "popc" then
            ;;; returns id to be used
            valof("popc_weakref_ident")(word, depend_vec, _weak_option) -> id
        elseif depend_vec then
            ;;; ensure all perm idents (need the extra procedure
            ;;; because of the caller(1) test below)
            procedure(depend_vec);
                lvars w, depend_vec;
                fast_for w in_vector depend_vec do
                    conspair(w, "weakref") -> w;
                    sys_use_current_ident(w) -> (,);
                    sys_grbg_list(w)
                endfor
            endprocedure(depend_vec);
            Note_perm_dependency(word, depend_vec)
        endif

    else
        ;;; not lexically or permanently declared
        if VM_LOADED and POP_SYNTAX_ONLY then
            ;;; just generate a dummy identifier
            ident dummy_lvar
        elseif caller(1) == sys_use_current_ident then
            ;;; already tried sysdeclare
            mishap(word, 1, 'NOT A PERMANENT OR LEXICAL IDENTIFIER')
        else
            ;;; try sysdeclare
            sysdeclare(word);
            sys_use_current_ident(word) ->  ;;; use _nonactive for this call
        endif -> id
    endif;

    id, _nonactive
enddefine;

define Passign_ident(token);
    lvars token, _idprops, (id, _nonactive) = sys_use_current_ident(token);
    unless (id!ID_IDENTPROPS ->> _idprops) _bitst _:M_ID_ACTIVE then
        true -> _nonactive
    endunless;
    ;;; lconstant or not
    _nonactive and _idprops _bitst _:M_ID_LEX
        and _idprops _bitst _:M_ID_CONSTANT,
    ;;; id
    id,
    ;;; word
    while ispair(token) do fast_front(token) -> token endwhile, token,
    ;;; nonactive
    _nonactive
enddefine;

define sys_current_val(/*token*/) with_nargs 1;
    lvars (id, _nonactive) = sys_use_current_ident(/*token*/, true);
    if vm_pas_mode == "popc" and not(id!ID_IDENTPROPS _bitst _:M_ID_LEX) then
        ;;; POPC shadow value for perm idents
        valof("popc_idval")(id, _nonactive)
    else
        if _nonactive then nonactive_idval(id) else idval(id) endif
    endif
enddefine;
;;;
define updaterof sys_current_val(/*item, token*/) with_nargs 2;
    lvars id, word, _nonactive;
    if (Passign_ident(/*token*/) -> (id, word, _nonactive)) then
        ;;; lconstant (VM must be loaded)
        VM_WEAK Passign_lconst((), id, word)
    else
        returnif(VM_LOADED and POP_SYNTAX_ONLY) (->);

        if vm_pas_mode == "popc"
        and not(id!ID_IDENTPROPS _bitst _:M_ID_LEX) then
            ;;; POPC shadow value for perm idents
            () -> valof("popc_idval")(id, _nonactive)
        elseif _nonactive then
            Nonactive_assign((), id, word)
        else
            () -> idval(id)
        endif
    endif
enddefine;

endsection;     /* $-Sys$-Vm */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 16 1996
        Added mechanism for fast to non-fast translation.
--- John Gibson, Aug 16 1993
        Replaced pop_p*as_mode with internal var vm_pas_mode
 */
