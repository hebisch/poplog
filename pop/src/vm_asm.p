/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/vm_asm.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;----------- VIRTUAL MACHINE -- ASSEMBLING THE PCR -----------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'vmdefs.ph'

constant
        procedure (maplist),
        _checkall, _checkplogall,
        _prolog_save_check, _prolog_unify_atom, _prolog_restore,
        _swap_out_continue, _swap_in_continue
    ;

vars
        procedure (pop_vm_exec_apply)
    ;

section $-Sys;

constant
        procedure (Make_idval_undef, Non_local_goto, Non_local_goto_id)
    ;

endsection;

section $-Sys$-Vm;

constant
        procedure (Assemble_pdr, Moveq_op, Lookup_nl_lab,
        Reprocess_instructions, Cons_lex_id, Cons_procedure,
        Cons_goto, Cons_push, Cons_pushq, Cons_pop, Cons_callabs,
        I_CREATE_SF, I_UNWIND_SF, I_RETURN, I_CHAINSUB, I_BR_std, I_CMP,
        I_ERASE, I_DLOCAL_CONTEXT, I_MOVE_CALLER_RETURN, I_FAST_+-
        )
    ;

weak constant
        procedure (I_SWITCH_base, I_SWITCH)
    ;

endsection;


;;; ------------------------------------------------------------------------

section $-Sys$-Vm;

lvars
    pdr_lab_lookup  = [],
    nl_lvar_lookup  = [],
    ;

define New_save_lvar(baseid) -> id;
    lvars id, p, baseid;
    New_lextoken() -> id;
    if baseid and vm_pas_mode then
        valof("pas_ident_token") -> p;
        p(baseid) -> p(id)
    endif
enddefine;


;;; --------------------------------------------------------------------

lconstant procedure Pcr_lvar_needed;

define lconstant Pcr_needs_closure(pcr);
    lvars id, home, pcr, lenv, lblk, save_lenv, rtid_args, _flags, _recurse,
        _result = false;
    if pcr!PCR_RTID_ARG_HOMES ->> rtid_args then
        ;;; been assembled
        returnif(rtid_args /== []) (true)
    else
        ;;; else we have to search the nonlocal lvar env
        pcr!PCR_LBLOCK -> lblk;
        lblk!LBLK_LVAR_ENV -> lenv;
        pcr!PCR_FLAGS &&/=_0 M_PCR_RECURSING -> _recurse;
        unless _recurse then
            pcr!PCR_FLAGS fi_|| M_PCR_RECURSING -> pcr!PCR_FLAGS;
            lenv -> save_lenv
        endunless;;

        until lenv == [] do
            dest_assoc(lenv) -> lenv -> id -> home;
            nextif(id!ID_IDENTPROPS _bitst _:M_ID_CONSTANT);
            id!ID_LEX_FLAGS -> _flags;
            if _flags _bitst _:M_LEX_USED
            and _flags _bitst _:M_LEX_NON_LOCAL
            and home!ID_LEX_FLAGS _bitst _:M_LEX_RTID_ACCESS
            then
                ;;; non-local, used and ref access
                if home!ID_LEX_FLAGS _bitst _:M_LEX_PCR_LVAR then
                    lenv -> lblk!LBLK_LVAR_ENV;
                    Pcr_lvar_needed(home) -> _result;
                    lblk!LBLK_LVAR_ENV -> lenv
                else
                    true -> _result
                endif;
                quitif(_result)
            endif
        enduntil;

        if _recurse then
            []
        else
            pcr!PCR_FLAGS fi_&&~~ M_PCR_RECURSING -> pcr!PCR_FLAGS;
            save_lenv
        endif -> lblk!LBLK_LVAR_ENV
    endif;

    if not(_result) and pcr!PD_UPDATER then
        Pcr_needs_closure(pcr!PD_UPDATER)
    else
        _result
    endif
enddefine;

define lconstant Pcr_lvar_needed(home);
    lvars home, pcr = fast_idval(home);
    unless Is_pcr(pcr) then
        ;;; if idval not a pcr, then it is used
        true
    elseunless home!ID_LEX_FLAGS _bitst _:M_LEX_USED then
        ;;; if not marked _:M_LEX_USED, then definitely not used
        false
    elseif Pcr_needs_closure(pcr) then
        ;;; needed
        0 -> fast_idval(home);          ;;; permanently mark needed
        true
    else
        ;;; not needed -- can clear USED flag
        home!ID_LEX_FLAGS _biclear _:M_LEX_USED -> home!ID_LEX_FLAGS;
        false
    endunless
enddefine;

define lconstant Assemble_sub_pcr(pcr);
    lvars id, pcr, d_list, rtid_args, rtid_arg_homes, uses_non_locals, props,
        nlgoto_var, clos_template, pdr, l_list, inner_l_lists, use_d_list,
        plab, _n, _nargs
        ;

    define lconstant asm_updater(pcr);
        lvars upd, pcr, clos_template, updpdr, pdr;
        returnunless(pcr!PD_UPDATER ->> upd);
        if pcr!PCR_CLOS_TEMPLATE ->> clos_template then
            if clos_template!PD_UPDATER then return endif;
        else
            Consclos_protect(identfn, 0) ->> clos_template
                                            -> pcr!PCR_CLOS_TEMPLATE;
            pcr!PD_PROPS -> clos_template!PD_PROPS
        endif;
        Assemble_sub_pcr(upd);
        upd!PCR_PDR_REC -> updpdr;
        if upd!PCR_RTID_ARG_HOMES /== [] then
            upd!PCR_CLOS_TEMPLATE
        else
            updpdr
        endif -> clos_template!PD_UPDATER;
        if isprocedure(pcr!PCR_PDR_REC ->> pdr) then
            updpdr -> pdr!PD_UPDATER;
            pdr -> clos_template!PD_CLOS_PDPART
        endif
    enddefine;

    define lconstant do_lblock(lblk, use_d_list);
        lvars id, home, lenv, l_list, short_rtids, full_rtids, clos_inits,
            lblk, use_d_list, _flags, _idprops
            ;
        lblk!LBLK_LVAR_ENV -> lenv;
        [] ->> l_list ->> inner_l_lists ->> short_rtids ->> full_rtids
            -> clos_inits;
        until lenv == [] do
            dest_assoc(lenv) -> lenv -> id -> home;
            id!ID_LEX_FLAGS -> _flags;
            id!ID_IDENTPROPS -> _idprops;

            ;;; don't need it if it's unused, an lconstant, the home lvar
            ;;; is an unused pcr lvar, or it's an nlgoto lvar that doesn't
            ;;; cross a push boundary...
            nextif( not(_flags _bitst _:M_LEX_USED)
                    or _idprops _bitst _:M_ID_CONSTANT
                    or (home!ID_LEX_FLAGS _bitst _:M_LEX_PCR_LVAR
                        and not(Pcr_lvar_needed(home)))
                    or (_flags _bitst _:M_LEX_NLGOTO_LVAR
                        and not(home!ID_LEX_FLAGS _bitst _:M_LEX_RTID_ACCESS))
                  );

            _CHECKINTERRUPT;

            if _flags _bitst _:M_LEX_NON_LOCAL then
                ;;; used non-local (outermost lblock only)
                true -> uses_non_locals;
                if home!ID_LEX_FLAGS _bitst _:M_LEX_RTID_ACCESS then
                    ;;; access is thru a run-time ident passed as an extra arg,
                    ;;; and we make this a local lvar
                    _flags _biset _:M_LEX_RTID_ACCESS -> id!ID_LEX_FLAGS;
                    id :: rtid_args -> rtid_args;
                    home :: rtid_arg_homes -> rtid_arg_homes
                else
                    ;;; access thru a dynamic var given by the idval;
                    ;;; this may be the home id, or that may be further
                    ;;; redirected
                    if home!ID_IDENTPROPS _bitst _:M_ID_LEX_TOKEN then
                        fast_idval(home)
                    else
                        home
                    endif -> fast_idval(id);
                    ;;; ensure USE_REG bit is clear
                    _flags _biclear _:M_LEX_USE_REG -> id!ID_LEX_FLAGS
                endif
            else
                ;;; local to this procedure
                if _flags _bitst _:M_LEX_RTID_ACCESS
                and not(_flags _bitst _:M_LEX_NLGOTO_LVAR) then
                    ;;; non-local use requires access via a run-time ident
                    if _idprops _bitst _:M_ID_ASSIGN_CHECK then
                        id :: full_rtids -> full_rtids
                    else
                        id :: short_rtids -> short_rtids
                    endif;
                    id :: l_list -> l_list
                elseif _flags _bitst _:M_LEX_USED_NON_LOCALLY then
                    ;;; non-local uses needs only a dynamic ident
                    if fast_back(use_d_list) /== []
                    and not(_idprops _bitst _:M_ID_ASSIGN_CHECK) then
                        ;;; reuse one thru the idval
                        fast_back(use_d_list) -> use_d_list;
                        fast_front(use_d_list) -> fast_idval(id);
                        ;;; ensure USE_REG bit is clear
                        _flags _biclear _:M_LEX_USE_REG -> id!ID_LEX_FLAGS
                    else
                        ;;; becomes a real identifier, no longer a token
                        _idprops _biclear _:M_ID_LEX_TOKEN -> id!ID_IDENTPROPS;
                        Make_idval_undef(id);
                        if _flags _bitst _:M_LEX_NLGOTO_LVAR then
                            ;;; This is the procedure's var used for non-local
                            ;;; gotos. Although all the descendants of the var
                            ;;; are marked as rtid access, this one is made a
                            ;;; dlocal and what actually gets passed down is
                            ;;; not an ident but an identifying pop integer
                            id :: d_list -> d_list;
                            id -> nlgoto_var
                        elseif _idprops _bitst _:M_ID_ASSIGN_CHECK then
                            id :: d_list -> d_list
                        else
                            ;;; this one is reuseable
                            id :: [] ->> fast_back(use_d_list) -> use_d_list
                        endif
                    endif
                else
                    ;;; straight local lvar
                    id :: l_list -> l_list
                endif;
                if _flags _bitst _:M_LEX_PCR_LVAR then
                    ;;; var to contain closure -- must be initialised to false
                    if _flags _bitst _:M_LEX_RTID_ACCESS then
                        ;;; ensure run-time ident can't be assigned to
                        id!ID_IDENTPROPS _biset _:M_ID_ASSIGNED_CONST -> id!ID_IDENTPROPS
                    endif;
                    id :: clos_inits -> clos_inits
                endif
            endif
        enduntil;

        lvars instr = lblk!LBLK_INSTR;
        if instr then
            ;;; assign values into the instruction starting the lblock
            short_rtids -> instr!INST_ARGS[_0];
            full_rtids -> instr!INST_ARGS[_1];
            clos_inits -> instr!INST_ARGS[_2]
        endif;

        l_list,
        [%  fast_for lblk in lblk!LBLK_INNER_BLOCKS do
                nonop nc_<>(do_lblock(lblk, use_d_list))
            endfor
        %]
    enddefine;      /* do_lblock */


    pcr!PD_PROPS -> props;
    ;;; nothing to do if already assembled
    if pcr!PCR_PDR_REC ->> pdr then
        if isprocedure(pdr) and pcr!PCR_FLAGS &&/=_0 M_PCR_RETURNED then
            ;;; in case pcr props got reassigned
            props -> pdr!PD_PROPS;
            if pcr!PCR_CLOS_TEMPLATE ->> clos_template then
                props -> clos_template!PD_PROPS
            endif
        endif;
        asm_updater(pcr);
        return
    endif;

    ;;; deal with lvars
    [] ->> d_list ->> rtid_args -> rtid_arg_homes;
    0 :: [] -> use_d_list;
    false ->> uses_non_locals -> nlgoto_var;
    do_lblock(pcr!PCR_LBLOCK, use_d_list) -> inner_l_lists -> l_list;
    d_list nc_<> fast_back(use_d_list) -> d_list;

    ;;; deal with dynamic locals
    lvars dloc_rtid_list = [], svid;
    fast_for id in pcr!PCR_DYN_VARS do
        if id!ID_IDENTPROPS _bitst _:M_ID_LEX_TOKEN then
            if id!ID_LEX_FLAGS _bitst _:M_LEX_RTID_ACCESS then
                ;;; must be a non-local run-time ident (i.e. in rtid_args)
                New_save_lvar(id) -> svid;
                l_list nc_<> [^svid] -> l_list;
                conspair(svid, id) :: dloc_rtid_list -> dloc_rtid_list;
                nextloop
            else
                ;;; transformed to dynamic ident -- make that local
                fast_idval(id) -> id
            endif
        endif;
        ;;; make it an ordinary dynamic local
        id :: d_list -> d_list
    endfor;

    ;;; deal with extra reference args passed for non-locals
    _pint(pcr!PD_NARGS) -> _nargs;
    if rtid_args /== [] then
        l_list nc_<> rtid_args -> l_list;
        rev(rtid_args) -> rtid_args;
        ;;; put a template closure in the pcr
        listlength(rtid_args) -> _n;
        _nargs fi_+ _n -> _nargs;       ;;; adjust _nargs
        Consclos_protect(identfn, fast_repeat _n times 0 endrepeat, _n)
                                ->> clos_template -> pcr!PCR_CLOS_TEMPLATE;
        props -> clos_template!PD_PROPS
    endif;
    rtid_arg_homes -> pcr!PCR_RTID_ARG_HOMES;
    if uses_non_locals then
        pcr!PCR_FLAGS fi_|| M_PCR_USES_NON_LOCALS -> pcr!PCR_FLAGS
    endif;

    if pcr!PCR_DLEXPR_LIST /== [] then
        ;;; Add any dlexpr save vars to end of lvar list.
        ;;; Front is list of vars, back is template codelists.
        l_list nc_<> (destpair(pcr!PCR_DLEXPR_LIST) -> pcr!PCR_DLEXPR_LIST)
                -> l_list
    endif;
    ;;; complete l_list
    l_list nc_<> inner_l_lists -> l_list;

    ;;; Construct an lconstant ident to contain the final procedure record
    ;;; necessary for assembling recursive calls of this procedure
    ;;; (this is put in a list, so others can be added)
    Cons_lex_id(_:M_ID_CONSTANT _biset _:M_ID_PROCEDURE, _0) :: []
                                                        -> pcr!PCR_PDR_REC;

    ;;; deal with updater
    asm_updater(pcr);

    ;;; Now deal with the instructions that need reprocessing
    ;;; (can lead to a recursive call of Assemble_sub_pcr)
    ;;; Put a dummy label at the end of codelist so that no instructions
    ;;; can get added at the end
    Newlab([]) ->> fast_back(pcr!PCR_CODE_END) -> pcr!PCR_CODE_END;
    Reprocess_instructions(pcr);

    ;;; produce the procedure record -- takes all these args!
    (   _nargs, props, d_list, l_list, nlgoto_var, rtid_args,
        pcr!PCR_LBLOCK!LBLK_INSTR, pcr!PCR_NON_LOCAL_LABELS,
        dloc_rtid_list, pcr!PCR_DLEXPR_LIST );
    if vm_pas_mode then
        ;;; replace label at end with pas_END
        Cons_inst("pas_END", 1) -> fast_front(pcr!PCR_CODE_END);
        valof("pas_assemble")(
                    (),                             ;;; args 1-10
                    ;;; procedure entry type arg    ;;; arg 11
                    if pcr!PCR_PLOG_PDR then 2
                    elseif pcr!PCR_FLAGS &&/=_0 M_PCR_ENTRY_CHECK then 1
                    else 0
                    endif,
                    fast_back(pcr!PCR_CODE_LIST)    ;;; arg 12
                             ) -> pdr;
        unless isprocedure(pdr) then
            mishap(pdr, 1, 'NON-PROCEDURE RETURNED BY pas_assemble')
        endunless
    else
        ;;; takes pcr instead of codelist
        Assemble_pdr((), pcr) -> pdr
    endif;

    ;;; fill procedure into things
    fast_for id in pcr!PCR_PDR_REC do
        id!ID_IDENTPROPS _biset _:M_ID_ASSIGNED_CONST -> id!ID_IDENTPROPS;
        pdr -> fast_idval(id)
    endfor;
    pdr -> pcr!PCR_PDR_REC;

    if pcr!PCR_CLOS_TEMPLATE ->> clos_template then
        pdr -> clos_template!PD_CLOS_PDPART
    endif;
    if pcr!PD_UPDATER then
        pcr!PD_UPDATER!PCR_PDR_REC -> pdr!PD_UPDATER
    endif;

    fast_for plab in pcr!PCR_NON_LOCAL_LABELS do
        pdr -> plab!PLAB_OWNER
    endfor;

    unless vm_pas_mode then
        ;;; garbage some stuff
        sys_grbg_list(l_list);
        sys_grbg_list(d_list);
    endunless
enddefine;

define Assemble_pcr(pcr);
    lvars pcr;
    dlocal pdr_lab_lookup = [];     ;;; pdrlab lookup list for non-local gotos
    Assemble_sub_pcr(pcr)
enddefine;



;;; --- REPROCESS (rI_ ) INSTRUCTIONS ----------------------------------------

define lconstant Lookup_home(home);
    lvars list, home;
    if list_assoc(home, nl_lvar_lookup) ->> list then
        ;;; the corresponding local identifer
        fast_front(list)
    else
        ;;; must be a (nested lblock) local
        home
    endif
enddefine;

define lconstant Push_rtid_args_code(pcr);
    lvars id, home, homes, pcr;
    pcr!PCR_RTID_ARG_HOMES -> homes;
    fast_for home in homes do
        ;;; needs a ref as argument -- push corresponding one in my env
        Lookup_home(home) -> id;
        if vm_pas_mode then
            Cons_inst(  if id!ID_IDENTPROPS _bitst _:M_ID_LEX_TOKEN then
                            "pas_IDENT"
                        else
                            "pas_PUSH"  ;;; this is for a home nlgoto lvar
                        endif, id, 2);
        else
            Cons_inst(  if id!ID_IDENTPROPS _bitst _:M_ID_LEX_TOKEN then
                            I_MOVEQ
                        else
                            I_MOVE  ;;; this is for a home nlgoto lvar
                        endif, id, 2);
        endif
    endfor;
    ;;; return true if there were some
    listlength(homes) /== 0
enddefine;

define lconstant Push_call_pcr(pcr, I_pdr_q, I_pdr);
    lvars pdr, pcr, I_pdr_q, I_pdr;
    if isprocedure(pcr!PCR_PDR_REC ->> pdr) then
        ;;; procedure record produced
        I_pdr_q
    else
        ;;; currently being compiled
        ;;; - pdr is a list with hd an ident that will contain the procedure
        fast_front(pdr) -> pdr;
        I_pdr
    endif;
    Cons_inst((), pdr, 2)
enddefine;

define lconstant Push_pcr(/*pcr*/) with_nargs 1;
    Push_call_pcr((),if vm_pas_mode then "pas_PUSHQ", "pas_PUSH"
                     else I_MOVEQ, I_MOVE
                     endif)
enddefine;

define lconstant Push_lex_pdr(pcr) -> push_code -> _need_clos;
    lvars pcr, push_code, _need_clos = false;
    ;;; code to push procedure or closure
    Assemble_sub_pcr(pcr);  ;;; make sure it's assembled
    [%
        if pcr!PD_UPDATER then
            Push_rtid_args_code(pcr!PD_UPDATER) -> _need_clos
        endif,
        if Push_rtid_args_code(pcr) then true -> _need_clos endif,
        if _need_clos then
            if vm_pas_mode then
                Cons_inst("pas_LEX_CLOSURE", pcr!PCR_CLOS_TEMPLATE, 2)
            else
                Cons_pushq(pcr!PCR_CLOS_TEMPLATE),
                Cons_callabs(Cons_lex_closure)
            endif
        else
            Push_pcr(pcr)
        endif
    %] -> push_code
enddefine;

define lconstant Try_lex_eval(instr) -> opnd;
    lvars opnd = instr!INST_ARGS[_0], id, instr, p;
    if opnd!KEY == ident_key then
        ;;; lconstant identifier -- must now be initialised
        opnd -> id;
        fast_idval(opnd) -> opnd
    else
        false -> id
    endif;
    returnunless(Is_pcr(opnd) and opnd!PCR_FLAGS &&/=_0 M_PCR_EVALUATE);

    Assemble_sub_pcr(opnd);     ;;; make sure it's assembled
    if isprocedure(opnd!PCR_PDR_REC ->> p) and opnd!PCR_RTID_ARG_HOMES == []
    then
        ;;; evaluate now
        if opnd!PCR_FLAGS &&/=_0 M_PCR_EVALUATED then
            opnd!PCR_LITERAL_CACHE -> opnd
        else
            opnd!PCR_FLAGS fi_|| M_PCR_EVALUATED -> opnd!PCR_FLAGS;
            pop_vm_exec_apply(p) ->> opnd!PCR_LITERAL_CACHE -> opnd;
            if p!PD_PROPS and isprocedure(opnd) then
                p!PD_PROPS -> opnd!PD_PROPS
            endif
        endif;
        if id then opnd -> fast_idval(id) endif
    else
        ;;; refers to itself recursively, or uses type-3 lvars
        mishap(0, 'ILLEGAL CODE FOR COMPILE-TIME EVALUATION')
    endif;
enddefine;

define rI_MOVEQ_LEX(instr);
    lvars   pcr, opnd  = Try_lex_eval(instr), id, instr, label, new_code,
            _need_clos;
    unless Is_pcr(opnd) then
        ;;; just push it
        opnd -> instr!INST_ARGS[_0];
        Moveq_op(opnd) -> instr!INST_OP;
        return(instr)
    endunless;

    ;;; pcr
    opnd -> pcr;

    ;;; code to push procedure or closure
    Push_lex_pdr(pcr) -> new_code -> _need_clos;

    if _need_clos then
        ;;; need variable to store closure to avoid multiple creation
        ;;; code sequence is  'id or (creat-closure ->> id)'
        Lookup_home(pcr!PCR_CLOS_LVAR) -> id;
        Newlab([]) -> label;
        if vm_pas_mode then
            [%  Cons_push(id),
                Cons_bool(label, true)              ;;; (OR label)
            %] nc_<> new_code nc_<>
            [%  Cons_inst("pas_PUSHS", 1),
                Cons_pop(id)
            %]
        else
            Cons_bool(label, true, Cons_push(id))   ;;; (OR label)
                :: new_code nc_<>
            [% Cons_inst(I_STORE, id, 2) %]
        endif nc_<> label -> new_code
    endif;

    if datalength(instr) == 3 then  ;;; not vm_pas_mode
        instr!INST_ARGS[_1] -> id;
        if _need_clos then
            new_code nc_<> (Cons_pop(id) :: []) -> new_code
        else
            ;;; must be just I_MOVE or I_MOVEQ something
            Cons_inst(explode(fast_front(new_code)), id, 3)
                    -> fast_front(new_code);
        endif;
    endif;
    new_code
enddefine;

define rI_MOVEI_LEX(instr);
    lvars id, instr, pcr, label, push_code, pdr;
    Try_lex_eval(instr) -> ;
    if vm_pas_mode then "pas_IDENT" else I_MOVEQ endif -> instr!INST_OP;
    instr!INST_ARGS[_0] -> id;  ;;; lconstant identifier, now initialised
    ;;; ensure its a real identifier
    id!ID_IDENTPROPS _biclear _:M_ID_LEX_TOKEN -> id!ID_IDENTPROPS;
    unless Is_pcr(fast_idval(id)) then
        ;;; just push the identifier
        return(instr)
    endunless;

    ;;; ensure pcr closure created if necessary
    fast_idval(id) -> pcr;  ;;; the pcr

    ;;; code to initialise clos lvar to closure if necessary
    if Push_lex_pdr(pcr) -> push_code then
        ;;; need variable to store closure to avoid multiple creation
        ;;; code sequence is 'unless id then (creat-closure -> id) endunless'
        Lookup_home(pcr!PCR_CLOS_LVAR) ->> id -> instr!INST_ARGS[_0];
        Newlab([]) -> label;
        if vm_pas_mode then
            [%  Cons_push(id),
                Cons_if(label, true)                ;;; (IFSO label)
            %] nc_<> push_code
        else
            Cons_if(label, true, Cons_push(id))     ;;; (IFSO label)
                :: push_code
        endif nc_<> [% Cons_pop(id) %] nc_<> label nc_<> [^instr]
    else
        ;;; ensure lconstant id is initialised to the real procedure
        if isprocedure(pcr!PCR_PDR_REC ->> pdr) then
            pdr -> fast_idval(id)
        else
            ;;; add to initialisation list
            unless fast_lmember(id, pdr) then
                pdr nc_<> [^id] -> pcr!PCR_PDR_REC
            endunless
        endif;
        instr
    endif
enddefine;

define lconstant Do_lex_call(instr, opnd, I_callpq, I_callp, I_callq);
    lvars opnd, instr, I_callpq, I_callp, I_callq;
    if Is_pcr(opnd) then
        ;;; pcr
        Assemble_sub_pcr(opnd);     ;;; make sure it's assembled
        ;;; return new code
        [%  Push_rtid_args_code(opnd) -> ,
            Push_call_pcr(opnd, I_callpq, I_callp),
        %]
    else
        ;;; just call it -- return same instruction
        opnd -> instr!INST_ARGS[_0];
        if isprocedure(opnd) then I_callpq else I_callq endif -> instr!INST_OP;
        instr
    endif;
enddefine;

define rI_CALLQ_LEX(instr);
    lvars opnd = Try_lex_eval(instr), instr;
    Do_lex_call(instr, opnd, if vm_pas_mode then
                                "pas_CALLQ", "pas_CALL", "pas_CALLQ"
                             else
                                I_CALLPQ, I_CALLP, I_CALLQ
                             endif)
enddefine;

define rI_UCALLQ_LEX(instr);
    lvars opnd = Try_lex_eval(instr), instr, upd;
    if isprocedure(opnd) then
        unless (opnd!PD_UPDATER ->> upd) then
            mishap(opnd, 1, 'LCONSTANT PROCEDURE HAS NO UPDATER')
        endunless;
        if Is_pcr(opnd) then
            ;;; pcr -- ensure base assembled first
            Assemble_sub_pcr(opnd)
        endif;
        upd -> instr!INST_ARGS[_0];
        rI_CALLQ_LEX(instr)
    else
        Do_lex_call(instr, opnd, if vm_pas_mode then
                                    "pas_UCALLQ", "pas_UCALL", "pas_UCALLQ"
                                 else
                                    I_UCALLPQ, I_UCALLP, I_UCALLQ
                                 endif)
    endif
enddefine;

define rI_CHAINQ_LEX(instr);
    lvars opnd = Try_lex_eval(instr), instr;
    [%  if Is_pcr(opnd) then
            ;;; pcr
            Assemble_sub_pcr(opnd);     ;;; make sure it's assembled
            Push_rtid_args_code(opnd) -> ;
            Push_pcr(opnd) -> instr         ;;; no lex closure needed
        else
            opnd -> instr!INST_ARGS[_0];
            Moveq_op(opnd) -> instr!INST_OP
        endif;
        if vm_pas_mode then
            instr, Cons_inst("pas_CALL", ident chain, 2)
        elseif isprocedure(opnd) then
            Cons_inst(I_CHAINPS, instr, 2)
        else
            instr, Cons_inst(I_CHAINS, 1)
        endif
    %]
enddefine;

define rI_NL_GOTO(instr);
    lvars nlgoto_home, plab, label, instr, id = false;
    ;;; deref target label
    Deref_lab(instr!INST_ARGS[_0]) -> label;
    ;;; get its procedure label structure
    Lookup_nl_lab(label, pdr_lab_lookup) -> plab;
    ;;; get the home var, now a dynamic (or false if target at execute level).
    plab!PLAB_IDENT -> nlgoto_home;
    if nlgoto_home and nlgoto_home!ID_LEX_FLAGS _bitst _:M_LEX_RTID_ACCESS then
        ;;; non-local goto crosses a push boundary -- this means the
        ;;; nlgoto lvar is actually needed, to identify the target
        ;;; stack frame to exit to
        Lookup_home(nlgoto_home) -> id
    else
        false -> plab!PLAB_IDENT
    endif;
    if vm_pas_mode then
        return(Cons_inst("pas_NL_GOTO", plab, id, 3))
    endif;
    [%
        Cons_pushq(plab),                       ;;; push plab
        if id then
            Cons_pushq(id),                     ;;; push the id
            Cons_callabs(Non_local_goto_id),    ;;; goto procedure
        else
            ;;; the nlgoto lvar isn't needed -- the exit is just to the first
            ;;; occurrence of the target procedure
            ;;; chain Non_local_goto
            Cons_inst(I_CHAINPS, Cons_pushq(Non_local_goto), 2)
        endif
    %]
enddefine;

define lconstant Do_reprocess(repinstrs);
    lvars new_code, codepair, instr, repinstrs;
    for codepair in repinstrs do
        fast_front(codepair) -> instr;
        ;;; the procedure returns a new instruction or a list of them
        fast_apply(instr, instr!INST_OP) -> new_code;
        ;;; insert new code in original code list
        if new_code == [] then
            ;;; no-op out instr
            Cons_inst(if vm_pas_mode then "pas_NO_OP" else identfn endif, 1)
                                            -> new_code
        elseif ispair(new_code) then
            fast_back(new_code) nc_<> fast_back(codepair) -> fast_back(codepair);
            fast_front(new_code) -> new_code
        endif;
        new_code -> fast_front(codepair)
    endfor;
    sys_grbg_list(repinstrs)
enddefine;

define rI_CONSTRUCT(instr) -> codelist;
    lvars   instr, n = 0, l, op, item, tmp, cons_p = false, _idprops,
            (, codelist, callstart, reprocess, type, count) = explode(instr);

    Do_reprocess(reprocess);

    if type == SCON_LIST then popstackmark endif;

    fast_for l on codelist do
        fast_front(l) -> instr;
        quitunless(instr!V_LENGTH == _2);
        instr!INST_OP -> op;
        instr!INST_ARGS[_0] -> item;
        if op == I_MOVEQ or op == "pas_PUSHQ" or op == "pas_IDENT" then
            if isident(item)
            and (item!ID_IDENTPROPS ->> _idprops) _bitst _:M_ID_LEX then
                if _idprops _bitst _:M_ID_LEX_TOKEN then
                    quitif(item!ID_LEX_FLAGS _bitst _:M_LEX_RTID_ACCESS);
                    fast_idval(item) -> item    ;;; deref to real id
                endif
            endif
        elseif op == "pas_PUSH" then
            quitunless(item!ID_IDENTPROPS _bitst _:M_ID_CONSTANT);
            valof("popc_idval")(item, true) -> item
        elseunless op == I_MOVENUM or op == I_MOVEADDR then
            if l == callstart and fast_back(l) == [] then
                if op == I_CALLABS or op == I_CALLPQ or op == I_CALLQ
                or op == "pas_CALLQ" then
                    item -> cons_p
                elseif op == "pas_CALL"
                and item!ID_IDENTPROPS _bitst _:M_ID_CONSTANT then
                    valof("popc_idval")(item, true) -> cons_p
                endif
            endif;
            quitloop
        endif;
        item;
        n fi_+ 1 -> n
    endfor;

    if cons_p and n == count then
        ;;; can construct now
        if type == SCON_VECONS then
            stacklength() fi_- count
        elseif type == SCON_VECTOR then
            count
        endif;
        Cons_pushq(cons_p()) -> codelist

    else
        ;;; must wait until runtime
        erasenum(n);
        if type == SCON_LIST then
            Cons_pushq(/*popstackmark*/) :: codelist -> codelist
        elseunless type == SCON_RECORD then
            [%  if type /== SCON_VECONS then
                    Cons_pushq(count)
                elseif vm_pas_mode then
                    Cons_inst("pas_CALL", ident stacklength, 2),
                    Cons_pushq(count),
                    Cons_inst("pas_CALL", ident fi_-, 2)
                else
                    Cons_callabs(stacklength),
                    Cons_inst(I_FAST_+-, false, Cons_pushq(count), false, 4)
                endif
            %] nc_<> callstart -> tmp;
            if codelist == callstart then
                tmp -> codelist
            else
                codelist -> l;
                until fast_back(l) == callstart do fast_back(l) -> l enduntil;
                tmp -> fast_back(l)
            endif
        endif
    endif
enddefine;

    ;;; do reprocessing instructions
define Reprocess_instructions(pcr);
    lvars pcr;
    ;;; these two dynamic vars are used non-locally in the rI_ instructions
    dlocal nl_lvar_lookup, pdr_lab_lookup;

    pcr!PCR_LBLOCK!LBLK_LVAR_ENV -> nl_lvar_lookup;
    pcr!PCR_NON_LOCAL_LABELS <> pdr_lab_lookup -> pdr_lab_lookup;
    ;;; list of instructions that need reprocessing
    Do_reprocess(pcr!PCR_REPROCESS_INSTRS);
    [] -> pcr!PCR_REPROCESS_INSTRS
enddefine;


;;; ----------------------------------------------------------------------

define lconstant Cons_move(src, dst);
    lvars src, dst;
    Cons_inst(I_MOVE, src, dst, 3)
enddefine;

define lconstant Norm_exit_code();
    Cons_inst(I_UNWIND_SF, 1), Cons_inst(I_RETURN, 1)
enddefine;

define lconstant Gen_trailing_code(dlexpr_list, dloc_rtid_list, initcode,
                                    l_list, crid)
                    -> (np_l_list, l_list, abexitlab, endcode, initcode);

    lvars   n, pair, initcode, endcode, abexitlab, normexitlab, l_list,
            np_l_list = [], dlexpr_save, crid, somedlexpr = false;

    dlvars  entry, dlexpr_list, dlexpr_index, dlexpr_proc, dloc_rtid_list,
            mystackmark, stackerr_lab;

    lconstant NORMAL = 1, ABNORMEXIT = 2, SUSPEND = 3, RESUME = 4;

    define lconstant copycode(_el, context);
        lvars opcode, instr, last, pair, codelist = entry!(w){_el}, start,
            context, _el;

        define lconstant do_dlcontext(instr, pair) -> (instr, pair);
            lvars next, pair, instr, _arg;
            if (fast_back(pair) ->> next) /== []
            and isvector(fast_front(next) ->> next)
            and next!INST_OP == I_CMP then
                unless next!INST_ARGS[_1] then
                    _1 -> _arg
                elseunless next!INST_ARGS[_2] then
                    _2 -> _arg
                else
                    return
                endunless;
                copy(next) -> next;
                instr -> next!INST_ARGS[_arg];
                next -> instr;
                fast_back(pair) -> pair
            endif
        enddefine;

        fast_for pair on codelist do
            unless isvector(fast_front(pair) ->> instr) then
                unless isinteger(instr) then
                    ;;; substituted with pair for previous copy
                    fast_front(instr) -> instr
                endunless;
                instr :: [] -> fast_front(pair)     ;;; Newlab
            endunless
        endfor;
        0 :: [] ->> last -> start;
        fast_for pair on codelist do
            unless isvector(fast_front(pair) ->> instr) then
                ;;; copied label
                instr
            else
                copy(instr) -> instr;
                instr!INST_OP -> opcode;
                if opcode == I_GOTO or opcode == I_IF or opcode = I_BOOL then
                    Deref_lab(instr!INST_ARGS[_0]) -> instr!INST_ARGS[_0]
                elseif opcode == I_GO_ON then
                    maplist(instr!INST_ARGS[_0], Deref_lab)
                                                    -> instr!INST_ARGS[_0];
                    if instr!INST_ARGS[_1] then
                        Deref_lab(instr!INST_ARGS[_1]) -> instr!INST_ARGS[_1]
                    endif
                elseif opcode == I_DLOCAL_CONTEXT then
                    if instr!INST_ARGS[_0] == "dlocal_context" then
                        ;;; substitute with push of context integer
                        do_dlcontext(Cons_pushq(context), pair) -> (instr, pair)
                    else
                        ;;; dlocal_process -- push proc being suspended/resumed
                        Cons_push(dlexpr_proc) -> instr
                    endif
                endif;
                instr :: []
            endunless ->> fast_back(last) -> last;
        endfor;
        fast_back(start)
    enddefine;

    define lconstant push_vars();
        applist(entry!DLX_VARS, Cons_push)
    enddefine;

    define lconstant pop_vars();
        applist(rev(entry!DLX_VARS), Cons_pop)
    enddefine;

    define lconstant set_index(i);
        lvars i;
        Cons_inst(I_MOVENUM, i, dlexpr_index, 3)
    enddefine;

    define lconstant switch_code(p, switch_var, proc);
        lvars n = 1, lab0, list, lablist, switch_var, proc, procedure p, opnd;
        [%  ;;; code to check mystackmark still in place
            Cons_inst(I_CMP, nonop _neq, Cons_pushq(mystackmark), false, 4),
            Cons_if(stackerr_lab, true),
            if proc then
                ;;; set chain_reg from dlexpr_proc, restore latter from stack
                Cons_move(dlexpr_proc, ident chain_reg),
                Cons_pop(dlexpr_proc)
            endif
        %].Newlab ->> lab0 -> list;

        if listlength(dlexpr_list) == 1 then
            [% Cons_inst(I_CMP, nonop _eq, Cons_pushq(0),
                                                    Cons_push(switch_var), 4),
               Cons_if(lab0, true)
            %] nc_<> p(1, lab0)
        else
            [% fast_for entry in dlexpr_list do
                 p(n, lab0).Newlab_ref1.dup nc_<> list -> list;
                 n fi_+ 1 -> n
            endfor %] -> lablist;           ;;; label list, lab 1 - lab N

            [%  ;;; switch on the index to restore the dlexprs saved
                Cons_push(switch_var) -> opnd;
                Cons_inst(if testdef I_SWITCH_base then
                            weakref I_SWITCH_base, lablist, true, opnd, 1, 5
                          else
                            weakref I_SWITCH, lablist, true, opnd, 4
                          endif);
                Cons_goto(lab0)             ;;; goto elselab if none to run
            %]
        endif nc_<> list -> list;

        [%  if proc then
                ;;; save current dlexpr_proc on stack and set to this proc
                Cons_push(dlexpr_proc),
                Cons_move(ident chain_reg, dlexpr_proc)
            endif,
            Cons_pushq(mystackmark)         ;;; stackmark
        %] nc_<> list
    enddefine;

    define lconstant swap_rtids();
        lvars pair;
        ;;; code to swap dlocal rt id values
        [%  fast_for pair in dloc_rtid_list do
                ;;; rtid, save -> rtid -> save
                Cons_push(fast_back(pair)),
                Cons_move(fast_destpair(pair)),
                Cons_pop(fast_front(pair))
            endfor
        %]
    enddefine;

    if dlexpr_list /== [] then
        ;;; Generate code for dlocal expressions. Format of entries in
        ;;; dlexpr_list is
        ;;;     { <save var list> <get code> <put code>}
        true -> somedlexpr;
        ;;; extra pop lvar used for saving process in suspend/resume code
        ;;; splice in at end of outer-level lvars
        New_lextoken() -> dlexpr_proc;
        l_list -> n;
        if n /== [] and isident(fast_front(n)) then
            while (fast_back(n) ->> pair) /== [] and isident(fast_front(pair))
            do pair -> n endwhile;
            dlexpr_proc :: pair -> fast_back(n)
        else
            dlexpr_proc :: n -> l_list
        endif;

        ;;; nonpop lvars used
        [% repeat 2 times New_lextoken() endrepeat %] -> np_l_list;
        dl(np_l_list) -> dlexpr_save    ;;; saves dlexpr_index
                     -> dlexpr_index;   ;;; indexes dlexprs run

        ;;; initialise index to 0 -- must be done at beginning of init code,
        ;;; before interrupt checking
        set_index(0) :: initcode -> initcode;

        Newlab(Cons_callabs(Dlexpr_stackerr) :: []) -> stackerr_lab;
        Get_record(stackmark_key) -> mystackmark
    endif;

    ;;; code for normal exit
    [%  ;;; restore dlocal rtids
        fast_for pair in dloc_rtid_list do
            ;;; front is save var, back is rt pair -- restore from save var
            Cons_move(fast_destpair(pair))
        endfor;
        ;;; normal unwind and return
        Norm_exit_code()
    %] . Newlab ->> endcode -> normexitlab;

    if somedlexpr then
        ;;; dlexpr code for normal entry -- goes at end of init code
        0 -> n;
        initcode,
        fast_for entry in dlexpr_list do
            () nc_<> copycode(@@DLX_GET_CODE, NORMAL) nc_<>
            [% pop_vars(), set_index(n fi_+ 1 ->> n) %]
        endfor -> initcode;

        ;;; dlexpr code for normal exit -- goes before normal end code.
        ;;; ensure it starts with a label so code optimisation can't
        ;;; mix up it up with the user code
        Newlab_ref1([]),
        fast_for entry in rev(dlexpr_list) do
            () nc_<> [% set_index(n fi_- 1 ->> n), push_vars() %]
            nc_<> copycode(@@DLX_PUT_CODE, NORMAL)
        endfor nc_<> endcode -> endcode
    endif;

    ;;; code for abnormal (chained) exit
    if somedlexpr then
        ;;; crid is a lextoken representing caller's return address

        ;;; exit code saves the frigged return in dlexpr_save and puts the
        ;;; proper return back, then switches on the index for the dlexprs,
        ;;; after which it puts the proper caller return back into CHAIN_REG
        ;;; and the frigged one into the return slot
        ;;; (then goes to normal exit)

        define lconstant mov_crid(src, dst);
            lvars src, dst;
            Cons_inst(I_MOVE_CALLER_RETURN, src, dst, I_MOVE, 4)
        enddefine;

        [% mov_crid(crid, dlexpr_save), mov_crid(ident chain_reg, crid) %]
            nc_<>
        switch_code(procedure(n, endlab);
                        lvars n, endlab;
                        [% set_index(n fi_- 1), push_vars() %]
                        nc_<> copycode(@@DLX_PUT_CODE, ABNORMEXIT)
                     endprocedure, dlexpr_index, false)
            nc_<>
         [% mov_crid(crid, ident chain_reg), mov_crid(dlexpr_save, crid),
            Cons_goto(normexitlab)
         %]
    else
        ;;; no dlexprs, must be some dlocal rt ids instead
        [% Cons_goto(normexitlab) %]
    endif . Newlab -> abexitlab;

    unless testdef process_key then
        ;;; code for processes not needed
        endcode nc_<> abexitlab -> endcode;
        return
    endunless;

    ;;; code to be run while suspending and resuming processes
    lvars lab, suslab, rsmlab;

    ;;; suspend first
    swap_rtids() nc_<> [% Cons_inst(I_CHAINSUB,
                            weakref[process_key] _swap_out_continue, 2) %]
                    -> lab;
    if somedlexpr then
        ;;; code saves the value of dlexpr index on the stack while running the
        ;;; expressions, and then pops it into dlexpr save (so
        ;;; that the resume code does upto that number and no more).
        Cons_pop(dlexpr_save) :: lab -> lab;
        Cons_push(dlexpr_index)
         ::
        switch_code(procedure(n, endlab);
                        lvars n, endlab;
                        [% set_index(n fi_- 1) %] nc_<>
                        copycode(@@DLX_GET_CODE, SUSPEND) nc_<>
                        [% push_vars() %] nc_<>
                        copycode(@@DLX_PUT_CODE, SUSPEND) nc_<>
                        [% pop_vars() %]
                    endprocedure, dlexpr_index, true)
            nc_<> lab -> lab
    endif;
    Newlab_ref1(lab) -> suslab;

    ;;; Then code for resume.
    [% Cons_inst(I_CHAINSUB,
                    weakref[process_key] _swap_in_continue, 2) %] -> lab;
    if somedlexpr then
        lab nc_<> stackerr_lab -> lab;
        ;;; do dlexprs upto index saved in dlexpr_save
        switch_code(procedure(n, endlab);
                        lvars i = 1, n, endlab;
                        [],
                        fast_for entry in dlexpr_list do
                            () nc_<>
                            [% push_vars() %] nc_<>
                            copycode(@@DLX_GET_CODE, RESUME) nc_<>
                            [% pop_vars() %] nc_<>
                            copycode(@@DLX_PUT_CODE, RESUME) nc_<>
                            [% set_index(i) %];
                            quitif(i == n);
                            i fi_+ 1 -> i
                        endfor,
                        if n /== 1 then () nc_<> [% Cons_goto(endlab) %] endif
                    endprocedure, dlexpr_save, true)
            nc_<> lab -> lab
    endif;
    Newlab_ref1(swap_rtids() nc_<> lab) -> rsmlab;

    ;;; All the trailing code from the chained exit label, whose address
    ;;; will be in the procedure's PD_EXIT
    abexitlab nc_<> suslab nc_<> rsmlab -> abexitlab;
    ;;; This is IMMEDIATELY preceded by two standard-length branch instructions
    ;;; of length L that go to the suspend code and the resume code.
    ;;; These are then accessible as
    ;;;         PD_EXIT - 2*L  and  PD_EXIT - L
    endcode nc_<>
        Newlab_ref1(Cons_inst(I_GOTO, suslab, I_BR_std, 3)
        :: Newlab_ref1(Cons_inst(I_GOTO, rsmlab, I_BR_std, 3) :: abexitlab))
            -> endcode
enddefine;

define Assemble_pdr(_nargs, props, d_list, l_list, nlgoto_var, rtid_args,
                    lblock_instr, plab_list, dloc_rtid_list, dlexpr_list,
                    pcr);
    lvars   id, pcr, initcode, endcode, exitlab, nlgoto_var,
            rtid_args, lblock_instr, props, plab_list, l_list, dlexpr_list,
            dloc_rtid_list, nonpop_l_list = [], pair, d_list, entry,
            _nargs, _pdr_flags = _0;

    ;;; decide which literals are put forward for possible reg allocation
    define lconstant select_literals(lit_assoc);
        lvars pair, lit_assoc, lit;
        ;;; A literal is represented by a pair whose front counts the
        ;;; number of times the literal is used, and whose back is
        ;;; the minimum number of usages for it to be register-cached.
        ;;; The pair is used in references to the literal (e.g. in
        ;;; I_CALLSUB_REG); we replace the front of the pair with the literal,
        ;;; which then gets replaced by a reg ident if it's allocated to one.
        ;;; (The back also gets replaced by the literal.)
        [%  until lit_assoc == [] do
                dest_assoc(lit_assoc) -> (lit, pair, lit_assoc);
                if fast_front(pair) fi_>= fast_back(pair) then
                    ;;; used sufficient number of times, put it forward
                    pair
                endif;
                lit ->> fast_front(pair) -> fast_back(pair)
            enduntil
        %]
    enddefine;

    ;;; Build extra code at beginning of procedure
    [%
        ;;; Code for extra run-time ident args passed for non-locals,
        ;;; followed by code to save any that are dlocals
        fast_for id in rtid_args do Cons_inst(I_POPQ, id, 2) endfor;
        fast_for pair in dloc_rtid_list do
            ;;; front is save var, back is rt pair -- save in save var
            Cons_move(fast_back(pair), fast_front(pair))
        endfor;

        ;;; Insert check at start of procedure. If this a prolog
        ;;; procedure (i.e. using PLOG_SAVE/RESTORE), then we use
        ;;; _prolog_save_check to save PLGSV values in the stack frame
        ;;; (which MUST be done before any GCs can happen inside
        ;;; this procedure).
        if pcr!PCR_PLOG_PDR then
            ;;; must call _prolog_save_check
            _pdr_flags _biset _:M_PD_PLOG_CHOICE -> _pdr_flags;
            Cons_inst(I_CALLSUB, weakref _prolog_save_check, 2)
        elseif pcr!PCR_FLAGS &&/=_0 M_PCR_ENTRY_CHECK then
            Cons_inst(I_CALLSUB,
                        if pcr!PCR_FLAGS &&/=_0 M_PCR_PLOG_TRAIL_CHECK then
                            weakref[prologvar_key] _checkplogall
                        else
                            _checkall
                        endif, 2)
        endif;

        ;;; I_LBLOCK instruction to set up rtids etc for outermost lblock
        if lblock_instr then lblock_instr endif;

        if nlgoto_var then
            ;;; code to initialise the local valof of the var to the next
            ;;; generated integer, which will uniquely identify this call
            ;;; (these integers wrap around, but circa 1000 million different
            ;;; ones for each procedure is unlikely to cause problems!)
            Cons_callabs(Gen_stack_frame_num),
            Cons_pop(nlgoto_var)
        endif;

    %] -> initcode;

    ;;; lextoken representing caller's return address
    lvars crid = New_lextoken();

    if dlexpr_list /== [] or dloc_rtid_list /== [] then
        Gen_trailing_code(dlexpr_list, dloc_rtid_list, initcode, l_list, crid)
                    -> (nonpop_l_list, l_list, exitlab, endcode, initcode);
        _pdr_flags _biset _:M_PD_PROC_DLEXPR_CODE -> _pdr_flags;

        ;;; weed out a separate list of dlocal active vars (that aren't just
        ;;; lextokens)
        [%  fast_for entry in dlexpr_list do
                nextunless(datalength(entry) == 4);
                entry!DLX_ACTID -> id;
                lconstant _LEX_TOKEN = _:M_ID_LEX _biset _:M_ID_LEX_TOKEN;
                nextif(id!ID_IDENTPROPS _bimask _LEX_TOKEN == _LEX_TOKEN);
                ;;; ensure their save lvars go in stack frame slots
                fast_for id in entry!DLX_VARS do
                    id!ID_LEX_FLAGS _biset _:M_LEX_DONT_USE_REG
                                -> id!ID_LEX_FLAGS
                endfor;
                entry
            endfor
        %] -> dlexpr_list
    else
        ;;; endcode is just normal exit
        [% Norm_exit_code() %].Newlab ->> endcode -> exitlab
    endif;

    ;;; produce procedure
    endcode -> fast_back(pcr!PCR_CODE_END);
    ;;; add a label at the end of initcode so code optimisation can't
    ;;; mix up it up with the user code
    Cons_procedure( initcode nc_<> Newlab_ref1(fast_back(pcr!PCR_CODE_LIST)),
                    d_list, _nargs, props, l_list, nonpop_l_list, plab_list,
                    exitlab, crid, select_literals(pcr!PCR_LITERAL_CACHE),
                    dlexpr_list, _pdr_flags) /* -> p */;

    ;;; Cons_procedure has garbaged the codelist and instruction vectors
    false ->> pcr!PCR_CODE_LIST ->> pcr!PCR_CODE_PENULT -> pcr!PCR_CODE_END;
    [] -> pcr!PCR_DYN_VARS
enddefine;

endsection;     /* $-Sys$-Vm */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 17 1996
        Now uses whichever of I_SWITCH or I_SWITCH_base is present.
--- John Gibson, Oct 11 1996
        Added some dummy labels to stop initial and end code getting
        mixed up with user code by code optimisation.
--- John Gibson, Sep 27 1996
        Changes to keep new label reference counts updated.
--- John Gibson, Jun 24 1995
        Added rI_CONSTRUCT, and made LEX reprocess instructions deal with
        'evaluate' pcrs (i.e. with M_PCR_EVALUATE set).
--- John Gibson, May 27 1995
        Changed Assemble_pdr to pass a list of dlocal active vars to
        Cons_procedure
--- John Gibson, Sep 19 1994
        Moved garbaging of instruction codelist/vectors to Cons_procedure
--- John Gibson, Aug 16 1993
        Replaced pop_p*as_mode with internal var vm_pas_mode
--- John Gibson, May  4 1993
        Moved Cons_ instruction procedures to vm_conspdr.p
--- John Gibson, Feb  5 1991
        Added interrupt check in -do_lblock-
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, May  3 1989
        Changes to -pas_assemble- interface.
--- John Gibson, Apr 30 1989
        Put into section $-Sys$-Vm.
--- John Gibson, Mar  1 1989
        Rewrote -Pcr_needs_closure- to cope better with recursion.
--- John Gibson, Nov 23 1988
        Changes for lexical blocks.
--- John Gibson, Jul 27 1988
        Now uses I_MOVE_CALLER_RETURN to move to/from caller's return
        address.
--- John Gibson, Jul 26 1988
        Changed handling of register-caching prolog subroutines
--- John Gibson, Mar 27 1988
        -list_assoc_val- into section Sys
--- John Gibson, Mar 11 1988
        Named changed to vm_asm.p (previously vmasm.p)
--- John Gibson, Feb 26 1988
        Weakref'ed prolog stuff
--- John Gibson, Feb 25 1988
        Weakref'ed process stuff in -Gen_trailing_code-
 */
