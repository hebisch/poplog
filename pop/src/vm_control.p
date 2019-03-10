/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/vm_control.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;---------------- VIRTUAL MACHINE CONTROL ------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'vmdefs.ph'

global constant
        procedure Sys$-Ensure_writeable
    ;

global vars
        pop_#_include_stack, $-Sys$-Prglst$- #_if_stack,
    ;

section $-Sys$-Vm;

constant
        procedure (Action_lex_pushes, Create_lex, Assemble_pcr,
        Get_goto_label_lblk, Plant_call, rI_NL_GOTO, I_LBLOCK
        Cons_push, Cons_pop, SysCALLQ, Begin_procedure, End_procedure
        )
    ;

vars
        _vm_stack_count
    ;

endsection;


;;; ----------------------------------------------------------------------

section $-Sys$-Vm =>
                    popexecute, pop_syntax_only, pop_pas_mode,
                    pop_vm_compiling_list, pop_vm_exec_apply,
                    pop_vm_dummy_idents, pop_new_lvar_list,
                    sysLBLOCK, sysENDLBLOCK, sysPROCEDURE, sysENDPROCEDURE,
                    sysEXECUTE, sysEXEC_COMPILE, sysEXEC_OPTION_COMPILE,
                    sysCOMPILE
                    ;

protected vars
    popexecute              = true,
    pop_vm_compiling_list   = [],
    procedure pop_vm_exec_apply = fast_apply,
    ;

vars
    pop_syntax_only     = false,
    pop_vm_dummy_idents = [],
    ;


vars
    vm_current_pcr      = false,
    vm_current_lblk     = false,
    vm_code_end         = writeable conspair(0, []),
    vm_code_penult      = false,
    vm_lex_assoc        = [],
    vm_lex_push_counts  = [],
    vm_label_count      = 0,
    vm_reprocess_instrs = [],
    vm_dlocal_context   = false,
    vm_dlocal_process   = false,
    _vm_level           = _0,
    ;


lconstant vm_nlgoto_lvar_token = 'nlgoto_lvar_token';

    ;;; a dynamic list of temporary lvar words
lvars tmp_lvar_num = 1;
;;;
lconstant
    tmp_lvars = writeable pdtolist(
                    procedure() -> word;
                        lvars word;
                        copy(consword('tmp_lvar_' sys_>< tmp_lvar_num))
                                                    -> word;
                        0 -> word!W_IDENTIFIER;
                        tmp_lvar_num fi_+ 1 -> tmp_lvar_num;
                    endprocedure
                );

vars
    pop_new_lvar_list = tmp_lvars;

define active pop_pas_mode;
    vm_pas_mode
enddefine;
;;;
define updaterof active pop_pas_mode newval;
    lvars newval, oldval = vm_pas_mode, id, p;
    returnif(newval == oldval);
    newval -> vm_pas_mode;
    if (isdefined("pop_change_pas_mode") ->> id)
    and not(isundef(fast_idval(id) ->> p)) then
        ;;; trap for popc
        p(oldval, newval)
    endif
enddefine;


;;; --- RESOLVING PROCEDURES -------------------------------------------

define lconstant Find_lvar(home_id, lblk) -> id;
    lvars lblk, id, home_id;
    repeat
        list_assoc_val(home_id, lblk!LBLK_LVAR_ENV) -> id;
        returnif(id or lblk!LBLK_LEVEL == 0);
        lblk!LBLK_OUTER_BLOCK -> lblk
    endrepeat
enddefine;

define lconstant Try_assemble(pcr);
    lvars pcr;

    define lconstant Trans_lconsts(lblk);
        lvars   lc_home, pcr_home, pcr_id, lc_id, lenv, save_list, pcr,
                lblk, _flags;
        lblk!LBLK_LVAR_ENV -> lenv;
        until lenv == [] do
            lenv -> save_list;
            dest_assoc(lenv) -> (lc_home, lc_id, lenv);
            nextunless( lc_home!ID_IDENTPROPS _bitst _:M_ID_CONSTANT
                        and Is_pcr(fast_idval(lc_home) ->> pcr)
                        and (pcr!PCR_CLOS_LVAR ->> pcr_home) );

            ;;; map lconstants containing pcrs onto pcr variables
            lc_id!ID_LEX_FLAGS -> _flags;
            unless Find_lvar(pcr_home, lblk) ->> pcr_id then
                ;;; pcr var not in this environment
                if _flags _bitst _:M_LEX_NON_LOCAL then
                    ;;; replace lconst home id with the pcr home
                    lc_id -> pcr_id;
                    pcr_home -> fast_front(save_list);
                    pcr_id!ID_IDENTPROPS _biclear _:M_ID_CONSTANT
                                                    -> pcr_id!ID_IDENTPROPS;
                    (pcr_home!ID_LEX_FLAGS _biclear _:M_LEX_USED)
                        _biset _:M_LEX_NON_LOCAL -> pcr_id!ID_LEX_FLAGS
                else
                    ;;; can't handle a local
                    mishap(0, 'SYSTEM ERROR IN Trans_lconsts')
                endif
            endunless;
            if _flags _bitst _:M_LEX_PUSHED then
                pcr_id!ID_LEX_FLAGS _biset _:M_LEX_USED -> pcr_id!ID_LEX_FLAGS
            endif;
            if lc_home!ID_LEX_FLAGS _bitst _:M_LEX_PUSHED then
                pcr_home!ID_LEX_FLAGS _biset _:M_LEX_USED _biset
                    ;;; in case identifier pushed with sysIDENT
                    (lc_home!ID_LEX_FLAGS _bimask _:M_LEX_RTID_ACCESS)
                        -> pcr_home!ID_LEX_FLAGS;
                lc_home!ID_IDENTPROPS _biclear (_:M_ID_CONSTANT
                    _biset _:M_ID_ASSIGNED_CONST) -> pcr_home!ID_IDENTPROPS
            endif
        enduntil;

        fast_for lblk in lblk!LBLK_INNER_BLOCKS do
            Trans_lconsts(lblk)
        endfast_for
    enddefine;      /* Trans_lconsts */

    lconstant INCOMPLETE = 0, COMPLETE = 1;

        /*  Can't assemble a pcr until this returns COMPLETE, i.e. when
            either there are no used non-locals, or they're all marked
            for rtid access
        */
    define lconstant Establish_var_status(pcr) -> _status;
        lvars id, home, pcr, lenv, _flags, _pushed, _status = COMPLETE;
        pcr!PCR_FLAGS &&/=_0 M_PCR_PUSHED -> _pushed;
        pcr!PCR_LBLOCK!LBLK_LVAR_ENV -> lenv;   ;;; only need non-locals
        until lenv == [] do
            dest_assoc(lenv) -> (home, id, lenv);
            nextif(id!ID_IDENTPROPS _bitst _:M_ID_CONSTANT);
            nextunless((id!ID_LEX_FLAGS ->> _flags) _bitst _:M_LEX_USED
                        and _flags _bitst _:M_LEX_NON_LOCAL);
            nextif((home!ID_LEX_FLAGS ->> _flags) _bitst _:M_LEX_RTID_ACCESS);
            ;;; lvar used non-locally, not already rtid access
            ;;; if pushed and not a dlvar, then becomes rtid
            if _pushed and not(_flags _bitst _:M_LEX_USE_DYNAMIC) then
                _flags _biset _:M_LEX_RTID_ACCESS -> home!ID_LEX_FLAGS
            else
                INCOMPLETE -> _status
            endif
        enduntil
    enddefine;      /* Establish_var_status */

    define lconstant Resolve_pcr(pcr, resolve_list);
        lvars   pcr, ur_list, next_ur_list, caller_lblk, item, rl,
                resolve_list, last, highest_recurse;

        define lconstant Match_lvars(caller_lblk0, caller_lblk, callee_lblk0);
            lvars id, home, cllr_id, callee_env, caller_lblk0, caller_lblk,
                callee_lblk0, caller_nlenv = false, _flags
                ;

            define lconstant Check_env_match(lblk, target_lblk);
                lvars lblk, target_lblk = target_lblk!LBLK_OUTER_BLOCK;
                while lblk!LBLK_OUTER_BLOCK ->> lblk do
                    returnif(lblk == target_lblk)
                endwhile;
                mishap(0, 'ENVIRONMENT MISMATCH IN Match_lvars')
            enddefine;

            callee_lblk0!LBLK_LVAR_ENV -> callee_env;
            until callee_env == [] do
                dest_assoc(callee_env) -> (home, id, callee_env);
                id!ID_LEX_FLAGS -> _flags;
                nextunless( _flags _bitst _:M_LEX_NON_LOCAL
                            and _flags _bitst _:M_LEX_USED
                            and not(id!ID_IDENTPROPS _bitst _:M_ID_CONSTANT) );
                ;;; lvar used non-locally
                if Find_lvar(home, caller_lblk) ->> cllr_id then
                    ;;; mark corresponding one used in caller
                    cllr_id!ID_LEX_FLAGS _biset _:M_LEX_USED
                                                    -> cllr_id!ID_LEX_FLAGS
                else
                    unless caller_nlenv then
                        ;;; first time -- check callee being used in the right
                        ;;; environment
                        Check_env_match(caller_lblk0, callee_lblk0);
                        ;;; OK to add new ones to caller nonlocal env
                        caller_lblk0!LBLK_LVAR_ENV -> caller_nlenv
                    endunless;
                    copy(id) -> cllr_id;
                    cons_assoc(home, cllr_id, caller_nlenv) -> caller_nlenv;
                    if vm_pas_mode then
                        valof("pas_ident_token")(id)
                                    -> valof("pas_ident_token")(cllr_id)
                    endif
                endif;
                ;;; mark home lvar used non-locally
                home!ID_LEX_FLAGS _biset
                    (_:M_LEX_USED _biset _:M_LEX_USED_NON_LOCALLY)
                                                -> home!ID_LEX_FLAGS
            enduntil;

            if caller_nlenv then
                caller_nlenv -> caller_lblk0!LBLK_LVAR_ENV
            endif
        enddefine;      /* Match_lvars */

        returnif(pcr!PCR_PDR_REC) (COMPLETE);   ;;; already assembled

        ;;; test if still waiting for non-local labels
        returnif(pcr!PCR_LABEL_COUNT fi_> 0) (false);

        Trans_lconsts(pcr!PCR_LBLOCK);
        if (pcr!PCR_UNRESOLVED_LIST ->> ur_list) == [] then
            chain(pcr, Establish_var_status)
        endif;

        conspair(pcr, resolve_list) -> resolve_list;
        false ->> highest_recurse -> last;
        until ur_list == [] do
            dest_assoc(ur_list) -> (item, caller_lblk, next_ur_list);
            if item!KEY == ident_key then
                ;;; lconstant - return false if not yet initialised
                returnunless(item!ID_IDENTPROPS _bitst _:M_ID_ASSIGNED_CONST)
                                                                    (false);
                if Is_pcr(fast_idval(item) ->> item) then
                    item -> fast_front(ur_list)
                else
                    ;;; not a pcr -- remove and ignore
                    next_ur_list ->> if last then fast_back(last)
                                     else pcr!PCR_UNRESOLVED_LIST
                                     endif -> ur_list;
                    nextloop
                endif
            endif;

            ;;; (item is now a pcr)
            if item!PD_UPDATER
            and not(list_assoc(item!PD_UPDATER, ur_list)) then
                cons_assoc(item!PD_UPDATER, caller_lblk, next_ur_list)
                                        -> fast_back(fast_back(ur_list))
            endif;

            ;;; return false unless a recursive cycle or pcr is resolved
            returnunless( (fast_lmember(item, resolve_list) ->> rl)
                            or (Resolve_pcr(item, resolve_list) ->> rl)
                        ) (false);

            fast_back(fast_back(ur_list)) -> next_ur_list;
            if isinteger(rl) /* INCOMPLETE or COMPLETE */ then
                ;;; item resolved -- remove from the list
                next_ur_list -> if last then fast_back(last)
                                else pcr!PCR_UNRESOLVED_LIST
                                endif
            else
                ;;; directly or indirectly recursive
                unless highest_recurse
                and fast_lmember(fast_front(highest_recurse), rl) then
                    rl -> highest_recurse
                endunless;
                fast_back(ur_list) -> last
            endif;
            next_ur_list -> ur_list;

            ;;; resolve the non-locally used lvars of
            ;;; the callee (item) with the environment of the caller (pcr)
            Match_lvars(pcr!PCR_LBLOCK, caller_lblk, item!PCR_LBLOCK)
        enduntil;

        if highest_recurse then
            ;;; contains recursive cycles
            if highest_recurse == resolve_list then
                ;;; this pcr is the highest (and is now complete)
                ;;; -- redo whole tree from here
                pcr!PCR_UNRESOLVED_LIST -> ur_list;
                [] -> pcr!PCR_UNRESOLVED_LIST;
                fast_back(resolve_list) -> resolve_list;    ;;; remove this pcr
                until ur_list == [] do
                    dest_assoc(ur_list) -> (item, , ur_list);
                    Resolve_pcr(item, resolve_list) ->
                enduntil
            else
                ;;; recurses to somewhere above
                return(highest_recurse)
            endif
        endif;

        chain(pcr, Establish_var_status);

compile_mode :vm +pentch;
    enddefine;      /* Resolve_pcr */

    if Resolve_pcr(pcr, []) == COMPLETE then
        Assemble_pcr(pcr)
    endif
enddefine;      /* Try_assemble */


;;; --- LEX BLOCKS --------------------------------------------------------

lconstant LBLOCK_LEN = _pint( ##(w)[_1|struct LBLOCK] _sub
                                        (##V_WORDS _sub ##POPBASE) );

define lconstant Cons_lblock(outer_lblk, level) -> lblk;
    lvars lblk = Ensure_writeable(initv(LBLOCK_LEN)), outer_lblk, level;
    outer_lblk  ->  lblk!LBLK_OUTER_BLOCK;
    level       ->  lblk!LBLK_LEVEL;
    []          ->> lblk!LBLK_LVAR_ENV
                ->> lblk!LBLK_LEX_ASSOC
                ->> lblk!LBLK_INNER_BLOCKS
                ->  lblk!LBLK_SYMBOL_LABELS;
    false       ->  lblk!LBLK_INSTR;
    pop_new_lvar_list   -> lblk!LBLK_TMP_LVARS;
    pop_vm_flags        -> lblk!LBLK_VM_FLAGS;
enddefine;

define lconstant Lblk_instr_args();
    if vm_pas_mode then "pas_LBLOCK" else I_LBLOCK endif, [], [], [], 4
enddefine;

define lconstant Save_current_lblk();
    lvars lblk = vm_current_lblk;
    vm_lex_assoc        -> lblk!LBLK_LEX_ASSOC;
    pop_new_lvar_list   -> lblk!LBLK_TMP_LVARS;
    pop_vm_flags        -> lblk!LBLK_VM_FLAGS;
enddefine;

define lconstant Restore_current_lblk();
    lvars lblk = vm_current_lblk;
    lblk!LBLK_LEX_ASSOC -> vm_lex_assoc;
    lblk!LBLK_TMP_LVARS -> pop_new_lvar_list;
    lblk!LBLK_VM_FLAGS  -> pop_vm_flags;
enddefine;

protected
define vars sysLBLOCK(executing);
    lvars lblk = vm_current_lblk, sub_lblk, executing;

    if executing then
        unless popexecute then
            mishap(0, 'sysLBLOCK: NOT AT EXECUTE LEVEL')
        endunless
    elseif popexecute then
        ;;; going from execute level to non-execute -- treat as a procedure
        Begin_procedure(false, 0, true);
        return
    endif;

    ;;; clear any lex pushes
    Action_lex_pushes();

    ;;; save the globals of the current lblock
    Save_current_lblk();

    ;;; initialise new sub lblock record
    Cons_lblock(lblk, lblk!LBLK_LEVEL fi_+ 1) ->> sub_lblk -> vm_current_lblk;

    unless popexecute then
        lblk!LBLK_INNER_BLOCKS nc_<> [^sub_lblk] -> lblk!LBLK_INNER_BLOCKS;
        ;;; plant the lblock instruction
        Plant(Lblk_instr_args());
        fast_front(vm_code_end) -> sub_lblk!LBLK_INSTR;
        _simstack _biclear _S_XPUSH -> _simstack
    endunless
enddefine;

define lconstant Check_lconsts(assoc_end);
    lvars id, list = vm_lex_assoc, word, assoc_end, _idprops, _count = 0;
    until list == assoc_end do
        dest_assoc(list) -> (word, id, list);
        id!ID_IDENTPROPS -> _idprops;
        if _idprops _bitst _:M_ID_CONSTANT
        and not(_idprops _bitst _:M_ID_ASSIGNED_CONST)
        and (popexecute or (_idprops _bitst _:M_ID_LEX_TOKEN
                            and not(id!ID_LEX_FLAGS _bitst _:M_LEX_NON_LOCAL)))
        then
            _count fi_+ 1 -> _count;
            word
        endif
    enduntil;
    if _count /== 0 then
        mishap(_count, 'UNINITIALISED LCONSTANT(S)')
    endif
enddefine;

define lconstant Pull_up_sym_labs(lblk, nonloc);
    lvars   label, sym, slabels = lblk!LBLK_SYMBOL_LABELS, lblk,
            to_lblk = lblk!LBLK_OUTER_BLOCK, nonloc,
            _flags, _nflags;
    until slabels == [] do
        dest_assoc(slabels) -> (sym, label, slabels);
        _int(fast_front(label)) -> _flags;
        unless _flags _bitst _:M_LAB_PLANTED then
            ;;; redirect the inner one to the outer one
            Get_goto_label_lblk(sym, not(nonloc), to_lblk)
                                    ->> fast_front(label) -> label;
            ;;; add the ref counts
            _int(fast_front(label)) -> _nflags;
            unless _nflags _bitst _:M_LAB_REFMAX then
                (_nflags _bimask _:M_LAB_REFCOUNT) _sub _:M_LAB_REF1
                    _add (_flags _bimask _:M_LAB_REFCOUNT) -> _flags;
                if _flags _gr _:M_LAB_REFMAX then
                    _:M_LAB_REFMAX -> _flags
                endif;
                _pint((_nflags _biclear _:M_LAB_REFCOUNT) _biset _flags)
                        -> fast_front(label)
            endunless;
            unless nonloc then
                ;;; correct label count coming out of an lblock
                vm_label_count fi_- 1 -> vm_label_count
            endunless
        endunless
    enduntil;
    [] -> lblk!LBLK_SYMBOL_LABELS
enddefine;

protected
define vars sysENDLBLOCK();
    lvars lblk = vm_current_lblk, outer_lblk = lblk!LBLK_OUTER_BLOCK;
    if lblk!LBLK_LEVEL == 0 then
        if vm_current_pcr!PCR_FLAGS &&/=_0 M_PCR_NONEXEC_LBLOCK then
            SysCALLQ(End_procedure(true));
            return
        else
            mishap(0, 'sysENDLBLOCK: NO MATCHING sysLBLOCK')
        endif
    endif;

    ;;; clear any lex pushes
    Action_lex_pushes();
    ;;; check all lconstants were defined
    Check_lconsts(outer_lblk!LBLK_LEX_ASSOC);

    Save_current_lblk();
    false -> lblk!LBLK_LEX_ASSOC;

    ;;; return to lblock context above
    outer_lblk -> vm_current_lblk;
    Restore_current_lblk();

    ;;; pull symbolic labels up to the outer block
    Pull_up_sym_labs(lblk, false)
enddefine;


;;; --- PCRS ---------------------------------------------------------------

lconstant PCR_LEN   = _pint( ##(w)[_1|struct PCR] _sub
                                        (##PD_CLOS_FROZVALS _sub ##POPBASE) );

define Pcr_pdr_complete(pcr) -> pdr;
    lvars pcr, pdr;
    unless (pcr!PCR_PDR_REC ->> pdr)
    and pcr!PCR_FLAGS &&=_0 M_PCR_USES_NON_LOCALS then
        false -> pdr
    endunless
enddefine;

define lconstant Pcr_base(self);
    lvars self, pdr;
    erasenum(PCR_LEN-1);        ;;; erase rest of pcr fields
    if Pcr_pdr_complete(self) ->> pdr then
        ;;; OK to run it if it's assembled and doesn't use non-locals
        fast_chain(pdr)
    else
        mishap(self, 1, 'APPLYING PROCEDURE COMPILATION RECORD')
    endif
enddefine;

define Is_pcr(item);
    lvars item;
    isprocedure(item) and pdpart(item) == Pcr_base
enddefine;

lvars pcr_template = false;
;;;
define lconstant Cons_pcr(outer_pcr, props, nargs, pcr_assoc, outer_lblk)
                                                                -> pcr;
    lvars pcr, props, nargs, pcr_assoc, outer_pcr, outer_lblk;
    unless pcr_template then
        Consclos_protect(Pcr_base, fast_repeat PCR_LEN times false endrepeat,
                                        PCR_LEN) -> pcr_template
    endunless;
    Ensure_writeable(copy(pcr_template)) -> pcr;

    props       ->  pcr!PD_PROPS;
    _int(nargs) ->  pcr!PD_NARGS;

    pcr         ->  pcr!PCR_SELF;       ;;; always the last field in the pcr
    outer_pcr   ->  pcr!PCR_OUTER_PCR;
    0           ->  pcr!PCR_FLAGS;
    0           ->  pcr!PCR_LABEL_COUNT;
    Ensure_writeable(0 :: []) ->> pcr!PCR_CODE_LIST -> pcr!PCR_CODE_END;
    []          ->  pcr!PCR_NON_LOCAL_LABELS;
    []          ->  pcr!PCR_DLEXPR_LIST;
    []          ->  pcr!PCR_DYN_VARS;
    []          ->  pcr!PCR_UNRESOLVED_LIST;
    []          ->  pcr!PCR_REPROCESS_INSTRS;
    pcr_assoc   ->  pcr!PCR_PCR_ASSOC;
    []          ->  pcr!PCR_LITERAL_CACHE;
    Cons_lblock(outer_lblk, 0) ->> pcr!PCR_LBLOCK -> pcr!PCR_CURR_LBLOCK
enddefine;

    ;;; allocate a pcr an lvar in the current environment for possible use
    ;;; in storing a lexical closure of its procedure
define lconstant Pcr_alloc_pcr_lvar(pcr);
    lvars pcr, id;
    Create_lex(pcr, _0, _0, false) -> id;
    id!ID_LEX_FLAGS _biset _:M_LEX_PCR_LVAR -> id!ID_LEX_FLAGS;
    id -> pcr!PCR_CLOS_LVAR;
    pcr -> fast_idval(id)
enddefine;

define Retrieve_pcr(p) -> p;
    lvars pcr, p;
    returnif(Is_pcr(p));
    unless list_assoc_val(p, vm_current_pcr!PCR_PCR_ASSOC) ->> pcr then
        Cons_pcr(false, false, 0, [], false) -> pcr;
        p -> pcr!PCR_PDR_REC;
        [] -> pcr!PCR_RTID_ARG_HOMES;
        Pcr_alloc_pcr_lvar(pcr)
    endunless;
    pdprops(p) -> pcr!PD_PROPS;
    _int(pdnargs(p)) -> pcr!PD_NARGS;
    pcr -> p
enddefine;

define lconstant Save_current_pcr();
    lvars pcr = vm_current_pcr;
    Save_current_lblk();
    vm_current_lblk     -> pcr!PCR_CURR_LBLOCK;
    vm_code_end         -> pcr!PCR_CODE_END;
    vm_code_penult      -> pcr!PCR_CODE_PENULT;
    vm_label_count      -> pcr!PCR_LABEL_COUNT;
    vm_reprocess_instrs -> pcr!PCR_REPROCESS_INSTRS;
enddefine;

define lconstant Restore_current_pcr();
    lvars pcr = vm_current_pcr;
    pcr!PCR_CODE_END        -> vm_code_end;
    pcr!PCR_CODE_PENULT     -> vm_code_penult;
    pcr!PCR_LABEL_COUNT     -> vm_label_count;
    pcr!PCR_REPROCESS_INSTRS -> vm_reprocess_instrs;
    pcr!PCR_CURR_LBLOCK     -> vm_current_lblk;
    Restore_current_lblk()
enddefine;


;;; --- STARTING A PROCEDURE ------------------------------------------------

define Begin_procedure(props, nargs, nonexec_lblock);
    lvars lid, id, props, nargs, nonexec_lblock, new_lvar_env, new_lex_assoc,
        _flags, _idprops
        ;
    Action_lex_pushes();            ;;; clear any lex pushes
    ;;; save the globals of the current pcr
    Save_current_pcr();

    ;;; pull lexical identifiers down, by creating new identifiers
    ;;; We build two association lists, one keyed on the name,
    ;;; the other keyed on the home identifier
    [] -> new_lvar_env;
    [%  until vm_lex_assoc == [] do
            ;;; (leave the token on the stack)
            dest_assoc(vm_lex_assoc) -> (id, vm_lex_assoc);
            copy(id) -> lid;
            if vm_pas_mode then
                ;;; attach token under which id declared
                dup() -> valof("pas_ident_token")(lid)
            endif;
            id -> fast_idval(lid);          ;;; chain the identifiers
            id!ID_LEX_FLAGS -> _flags;
            id!ID_IDENTPROPS -> _idprops;
            if _idprops _bitst _:M_ID_CONSTANT then
                _flags _biclear _:M_LEX_PUSHED -> _flags
            endif;
            (_flags _biset _:M_LEX_NON_LOCAL) _biclear _:M_LEX_USED
                                                        -> lid!ID_LEX_FLAGS;
            if _idprops _bitst _:M_ID_LEX_TOKEN then
                ;;; home id is a local, i.e. not a real identifier
                ;;; get home id first
                while _flags _bitst _:M_LEX_NON_LOCAL do
                    fast_idval(id) -> id;
                    id!ID_LEX_FLAGS -> _flags
                endwhile;
                ;;; add entry to lvar environment
                id :: (lid :: new_lvar_env) -> new_lvar_env
            endif;
            lid
        enduntil
    %] -> new_lex_assoc;

    ;;; no longer at execute level
    _vm_level _add _1 -> _vm_level;
    false -> popexecute;

    ;;; initialise new pcr
    Cons_pcr(vm_current_pcr, props, nargs, vm_current_pcr!PCR_PCR_ASSOC,
                                    vm_current_lblk) -> vm_current_pcr;
    Restore_current_pcr();
    new_lvar_env -> vm_current_lblk!LBLK_LVAR_ENV;
    new_lex_assoc -> vm_lex_assoc;
    Cons_inst(Lblk_instr_args()) -> vm_current_lblk!LBLK_INSTR;

    _S_NONE -> _simstack;
    _0 -> _vm_stack_count;

    ;;; create a special lvar for (possible) use with non-local gotos
    Create_lex(vm_nlgoto_lvar_token, _0, _0, false) -> id;
    id!ID_LEX_FLAGS _biset _:M_LEX_NLGOTO_LVAR -> id!ID_LEX_FLAGS;

    if nonexec_lblock then
        vm_current_pcr!PCR_FLAGS fi_|| M_PCR_NONEXEC_LBLOCK
                                                -> vm_current_pcr!PCR_FLAGS
    else
        ;;; a 'real' procedure -- add to pop_vm_compiling_list
        vm_current_pcr :: pop_vm_compiling_list -> pop_vm_compiling_list
    endif
enddefine;

protected
define vars sysPROCEDURE = Begin_procedure(% false %) enddefine;


;;; --- FINISHING A PROCEDURE ----------------------------------------------

    ;;; collect up all save vars for dlocal exprs
define lconstant Do_dlexprs();
    lvars entry, list, dlexpr_list = [], all_save_vars = [];
    vm_current_pcr!PCR_DLEXPR_LIST -> list;
    until list == [] do
        dest_assoc(list) -> (, entry, list);        ;;; discard key item
        copylist(entry!DLX_VARS) nc_<> all_save_vars -> all_save_vars;
        entry :: dlexpr_list -> dlexpr_list
    enduntil;
    conspair(all_save_vars, dlexpr_list) -> vm_current_pcr!PCR_DLEXPR_LIST
enddefine;

    ;;; look up a (non-local) label in a procedure label list
define Lookup_nl_lab(nl_lab, plab_list);
    lvars plab, nl_lab, plab_list;
    fast_for plab in plab_list do
        returnif(plab!PLAB_LABEL == nl_lab) (plab)
    endfast_for;
    false
enddefine;


    ;;; deal with non-local labels that got planted in the current procedure
define lconstant Do_nl_labs(nl_labs) -> (my_plabs, nl_labs);
    lvars id, label, nl_labs, nlgoto_lvar, pcr_using, my_plabs = [], plab,
        _flags;
    ;;; get this procedures's nlgoto lvar (will be <false> at execute level)
    list_assoc_val(vm_nlgoto_lvar_token, vm_lex_assoc) -> nlgoto_lvar;
    ;;; go thru non-local label list
    [% until nl_labs == [] do
        dest_assoc(nl_labs) -> (label, pcr_using, nl_labs);
        Deref_lab(label) -> label;
        _int(fast_front(label)) -> _flags;
        unless _flags _bitst _:M_LAB_PLANTED
        and _flags _bimask _:M_LAB_LEVEL == _vm_level then
            ;;; just put back on new vm_non_local_labels list
            label, pcr_using;
            nextloop
        endunless;

        ;;; else it's a label belonging to the current procedure.

        ;;; Add a procedure label record to the current procedure's list.
        ;;; This record is the thing used in actually executing a
        ;;; non-local goto. While the procedure is being compiled, the
        ;;; PLAB_LABEL field contains the label pair; when compiled the
        ;;; PLAB_OFFSET is filled in with the offset from PD_EXECUTE to
        ;;; the code position (and the PLAB_OWNER is filled in also).
        unless Lookup_nl_lab(label, my_plabs) then
            Get_record(procedure_label_key) -> plab;
            false -> plab!PLAB_OWNER;
            label -> plab!PLAB_LABEL;
            nlgoto_lvar -> plab!PLAB_IDENT;
            plab :: my_plabs -> my_plabs
        endunless;

        ;;; Decrement the using pcr's outstanding nl label count
        pcr_using!PCR_LABEL_COUNT fi_- 1 -> pcr_using!PCR_LABEL_COUNT;

        ;;; Unless at execute level, or this is a sysDLABEL label,
        ;;; for the pcr using the label non-locally mark the descendant
        ;;; lvars of this procedures's nlgoto lvar used
        nextif(not(nlgoto_lvar) or _flags _bitst _:M_LAB_DLABEL);
        list_assoc_val(nlgoto_lvar, pcr_using!PCR_LBLOCK!LBLK_LVAR_ENV)
                                                                -> id;
        ;;; mark used all the way up the chain
        repeat
            id!ID_LEX_FLAGS -> _flags;
            quitif(_flags _bitst _:M_LEX_USED);
            _flags _biset _:M_LEX_USED -> id!ID_LEX_FLAGS;
            quitunless(_flags _bitst _:M_LEX_NON_LOCAL)
        endrepeat;
    enduntil %] -> nl_labs
enddefine;

    /*  Reprocess the codelist, substituting non-local goto instructions
        where needed, and producing an assoc list of non-local labels
        against the current pcr.
    */
define lconstant Insert_nlgotos(list, nl_labs) -> (new_endpair, nl_labs);
    lvars   ipdr, instr, codepair, label, nextpair, list, pair,
            new_endpair = false, _GOTO, _IF, _BOOL, _GO_ON;
    dlvars  nl_labs;

    define lconstant is_nl_lab(label) -> label;
        lvars label, _flags;
        Deref_lab(label) -> label;
        _int(fast_front(label)) -> _flags;
        if _flags _bitst _:M_LAB_PLANTED
        and _flags _bimask _:M_LAB_LEVEL == _vm_level then
            ;;; local label
            false -> label
        else
            ;;; is non-local -- add current pcr to labs list
            unless list_assoc(label, nl_labs) then
                label :: (vm_current_pcr :: nl_labs) -> nl_labs
            endunless
        endif
    enddefine;

    define lconstant add_nlgoto(label, clist, addlab) -> clist;
        lvars label, clist, addlab;
        Cons_inst(rI_NL_GOTO, label, 2) :: clist -> clist;
        ;;; remember on reprocessing list
        clist :: vm_reprocess_instrs -> vm_reprocess_instrs;
        if addlab then Newlab_ref1(clist) -> clist endif
    enddefine;

    if vm_pas_mode then
        "pas_GOTO" -> _GOTO, "pas_IF" -> _IF,
        "pas_BOOL" -> _BOOL, "pas_GO_ON" -> _GO_ON
    else
        I_GOTO -> _GOTO, I_IF -> _IF,
        I_BOOL -> _BOOL, I_GO_ON -> _GO_ON
    endif;

    until (list ->> codepair) == [] do
        destpair(codepair) ->> list -> nextpair -> instr;
        nextunless(isvector(instr));
        _CHECKINTERRUPT;
        instr!INST_OP -> ipdr;
        if     ipdr == _GOTO then
            nextunless(is_nl_lab(instr!INST_ARGS[_0]) ->> label);
            label -> instr!INST_ARGS[_0];
            rI_NL_GOTO -> instr!INST_OP;
            ;;; remember on reprocessing list
            codepair :: vm_reprocess_instrs -> vm_reprocess_instrs
        elseif ipdr == _IF or ipdr == _BOOL or ipdr == I_NBOOL then
            nextunless(is_nl_lab(instr!INST_ARGS[_0]) ->> label);
            if isvector(fast_front(nextpair ->> pair)) then
                Newlab(pair) -> pair
            endif;
            if ipdr == _IF then
                not(instr!INST_ARGS[_1]) -> instr!INST_ARGS[_1];    ;;; reverse test
                pair -> instr!INST_ARGS[_0];
                Incr_lab_refcount(pair);
                add_nlgoto(label, pair, false) -> nextpair
            else
                ;;; BOOL, NBOOL
                add_nlgoto(label, pair, true) ->> nextpair
                                                -> instr!INST_ARGS[_0];
                Cons_goto(pair) :: nextpair -> nextpair
            endif
        elseif ipdr == _GO_ON then
            ;;; insert extra non-local gotos where necessary
            fast_for pair on instr!INST_ARGS[_0] do     ;;; i.e. on lablist
                nextunless(is_nl_lab(fast_front(pair)) ->> label);
                add_nlgoto(label, nextpair, true) ->> nextpair -> fast_front(pair);
            endfor;
            if (instr!INST_ARGS[_1] ->> label)
            and (is_nl_lab(label) ->> label) then
                ;;; has non-local elselab
                add_nlgoto(label, nextpair, true) ->> nextpair -> instr!INST_ARGS[_1]
            endif
        endif;

        nextpair -> fast_back(codepair);
        if list == [] and nextpair /== [] then
            ;;; return new end pair of list
            until (fast_back(nextpair) ->> pair) == [] do
                pair -> nextpair
            enduntil;
            nextpair -> new_endpair
        endif
    enduntil
enddefine;

    ;;; Deal with non-local labels/gotos, returning a list of
    ;;; outstanding non-local labels
define lconstant Get_outstanding_labs() -> nl_labs;
    lvars new_nl_labs, entry, list, new_endpair, nl_labs, label, slabels,
        sym, missing_slabs = [], done = [], _labcount = 0
        ;
    ;;; Deal with non-local labels that got planted in the current procedure
    ;;; Remainder are returned in a new list, together with the list of
    ;;; procedure labels for the current procedure.
    Do_nl_labs(vm_current_pcr!PCR_NON_LOCAL_LABELS)
                            -> (vm_current_pcr!PCR_NON_LOCAL_LABELS, nl_labs);

    ;;; if any labels gone to haven't been planted, they must be
    ;;; non-local gotos
    if vm_label_count /== 0 then
        ;;; insert nlgotos for main codelist
        Insert_nlgotos(tl(vm_current_pcr!PCR_CODE_LIST), [])
                                            -> (new_endpair, new_nl_labs);
        if new_endpair then new_endpair -> vm_code_end endif;
        unless popexecute then
            ;;; insert nlgotos for dlexpr codelists
            vm_current_pcr!PCR_DLEXPR_LIST -> list;
            until list == [] do
                dest_assoc(list) -> (, entry, list);
                Insert_nlgotos(entry!DLX_GET_CODE, new_nl_labs) -> (, new_nl_labs);
                Insert_nlgotos(entry!DLX_PUT_CODE, new_nl_labs) -> (, new_nl_labs)
            enduntil
        endunless;
        listlength(new_nl_labs) fi_>> 1 -> vm_label_count;
        new_nl_labs nc_<> nl_labs -> nl_labs
    endif
enddefine;

    ;;; transfer non-local labels up to outer level
define lconstant Pull_up_nl_labs(nl_labs);
    lvars label, nl_labs, _flags;
    ;;; transfer non-local labels up to outer level (including
    ;;; dereferencing symbolic labels pulled up)
    until nl_labs == [] do
        ;;; deref label
        Deref_lab(fast_front(nl_labs)) -> label;
        _int(fast_front(label)) -> _flags;
        ;;; ensure label marked at current level or above
        if _flags _bimask _:M_LAB_LEVEL _gr _vm_level then
            _pint(_flags _biclear _:M_LAB_LEVEL _biset _vm_level)
                -> fast_front(label)
        endif;
        label -> fast_front(nl_labs);
        fast_back(fast_back(nl_labs)) -> nl_labs
    enduntil
enddefine;

define End_procedure(nonexec_lblock);
    lvars pcr, pdr, this_pcr = vm_current_pcr, this_lblk = vm_current_lblk,
        outstanding_nl_labs, nonexec_lblock
        ;

    unless nonexec_lblock then
        if popexecute then
            mishap(0, 'sysENDPROCEDURE: NO MATCHING sysPROCEDURE')
        elseif this_lblk!LBLK_LEVEL /== 0 then
            mishap(0, 'sysENDPROCEDURE: MISSING sysENDLBLOCK')
        endif;
        tl(pop_vm_compiling_list) -> pop_vm_compiling_list
    endunless;

    ;;; Deal with non-local labels/gotos, returning a list of
    ;;; outstanding non-local labels
    Get_outstanding_labs() -> outstanding_nl_labs;

    ;;; deal with PCR_DLEXPR_LIST
    Do_dlexprs();

    ;;; clear any lex pushes
    Action_lex_pushes();
    ;;; check all lconstants were defined
    Check_lconsts([]);

    Save_current_pcr();
    false -> this_lblk!LBLK_LEX_ASSOC;
    sys_grbg_list(vm_lex_assoc);

    if pop_debugging or pop_vm_flags &&=_0 VM_NO_PDR_ENTRY_CHECKS then
        this_pcr!PCR_FLAGS fi_|| M_PCR_ENTRY_CHECK -> this_pcr!PCR_FLAGS
    endif;

    ;;; return to the outer procedure
    this_pcr!PCR_OUTER_PCR ->> pcr -> vm_current_pcr;
    Restore_current_pcr();
    _vm_level _sub _1 -> _vm_level;
    _zero(_vm_level) -> popexecute;
    _S_NONE -> _simstack;
    _0 -> _vm_stack_count;

    if outstanding_nl_labs /== [] then
        ;;; bind non-local symbolic labels in the outer level
        Pull_up_sym_labs(this_lblk, true);
        ;;; transfer non-local labels up to outer level
        Pull_up_nl_labs(outstanding_nl_labs);
        outstanding_nl_labs nc_<> pcr!PCR_NON_LOCAL_LABELS
                                            -> pcr!PCR_NON_LOCAL_LABELS
    endif;

    unless popexecute then
        ;;; allocate the pcr an lvar in the outer environment for possible use
        ;;; in storing a lexical closure of this procedure
        Pcr_alloc_pcr_lvar(this_pcr)
    endunless;

    if pop_syntax_only then
        identfn
    elseif outstanding_nl_labs == []
    and (Try_assemble(this_pcr), Pcr_pdr_complete(this_pcr) ->> pdr) then
        ;;; It's been assembled, and doesn't use any labels or lvars
        ;;; non-locally so we can produce the procedure.
        ;;; Add the pdr/pcr to PCR_ASSOC so that the pcr can be recovered
        ;;; from the procedure if later necessary
        unless popexecute then
            pdr :: (this_pcr :: pcr!PCR_PCR_ASSOC) -> pcr!PCR_PCR_ASSOC
        endunless;
        pdr
    else
        ;;; return the pcr
        this_pcr!PCR_FLAGS fi_|| M_PCR_RETURNED -> this_pcr!PCR_FLAGS;
        this_pcr
    endif
enddefine;      /* End_procedure */

protected
define vars sysENDPROCEDURE = End_procedure(% false %) enddefine;


;;; --- EXECUTE LEVEL CODE -----------------------------------------------

define Code_empty();
    vm_current_pcr!PCR_CODE_LIST == vm_code_end
enddefine;

    ;;; Deal with missing labels
define lconstant Missing_labs(missing_labs);
    lvars   missing_labs, label, slabels, sym, missing_slabs = [], done = [],
            _labcount = 0;
    ;;; weed out missing symbol labels first
    vm_current_pcr!PCR_LBLOCK!LBLK_SYMBOL_LABELS -> slabels;
    until slabels == [] do
        dest_assoc(slabels) -> (sym, label, slabels);
        unless _int(fast_front(label)) _bitst _:M_LAB_PLANTED then
            sym :: missing_slabs -> missing_slabs;
            label :: done -> done
        endunless
    enduntil;
    ;;; now count the missing direct labels
    until missing_labs == [] do
        unless lmember(dest_assoc(missing_labs) -> (, missing_labs), done)
        then
            _labcount fi_+ 1 -> _labcount
        endunless
    enduntil;
    if _labcount /== 0 then
        missing_slabs
            nc_<> [%consword('system-labels(' sys_>< _labcount <> ')')%]
                    -> missing_slabs
    endif;
    ;;; finally, the mishap
    mishap(dl(missing_slabs), length(missing_slabs), 'UNDEFINED LABEL(S)',
                                    'vm-label:name-ref-none')
enddefine;

protected
define vars sysEXECUTE();
    lvars pcr = vm_current_pcr, lblk = vm_current_lblk, l, tmp, procedure p;
    unless popexecute then
        mishap(0, 'sysEXECUTE: NOT AT EXECUTE LEVEL')
    endunless;

    until lblk!LBLK_LEVEL == 0 do
        Pull_up_sym_labs(lblk, false);
        lblk!LBLK_OUTER_BLOCK -> lblk
    enduntil;

    if (Get_outstanding_labs() ->> l) /== [] then
        ;;; can't be any labels outstanding
        Missing_labs(l)
    endif;
    if pop_syntax_only or Code_empty() then
        identfn -> p
    else
        Save_current_pcr();
        pcr!PCR_DLEXPR_LIST -> tmp, [] -> pcr!PCR_DLEXPR_LIST;
        Try_assemble(pcr);
        tmp -> pcr!PCR_DLEXPR_LIST;
        ;;; if not assembled then can't execute the code yet
        returnunless(pcr!PCR_PDR_REC ->> p)
    endif;

    ;;; re-initialise pcr/lblk
    [] ->> lblk!LBLK_SYMBOL_LABELS -> pcr!PCR_NON_LOCAL_LABELS;
    Ensure_writeable(0 :: []) ->> pcr!PCR_CODE_LIST -> vm_code_end;
    false ->> pcr!PCR_PDR_REC -> vm_code_penult;
    [] -> vm_reprocess_instrs;

    _S_NONE -> _simstack;
    _0 -> _vm_stack_count;

    ;;; run the procedure with pop_vm_exec_apply (defaults to fast_apply,
    ;;; but redefined by Popc).
    pop_vm_exec_apply(p);

    ;;; NOTE that a sys_lock_system could have happened ...
enddefine;

    /*  Compile_code with compile_p, and then execute.
    */
protected
define vars sysEXEC_COMPILE(compile_p, flags);
    lvars   compile_p, flags, return_result, result, p;
    dlocal  pop_new_lvar_list, pop_syntax_only = false,
            pop_vm_dummy_idents = [];
    lconstant macro (RETURN_RESULT = 2:01, ALLOW_DELAYED_EVAL = 2:10);

    if isboolean(flags) then
        if flags then RETURN_RESULT else 0 endif -> flags
    else
        Check_integer(flags, 0)
    endif;
    flags &&/=_0 RETURN_RESULT -> return_result;

    if popexecute and Code_empty() then
        ;;; OK to use sysEXECUTE
        compile_p(), if return_result then () -> result endif;
        sysEXECUTE()
    else
        ;;; must construct procedure
        sysPROCEDURE(false, 0);
        ;;; compile code
        compile_p(), if return_result then () -> result endif;
        unless Is_pcr(sysENDPROCEDURE() ->> p) then
            ;;; run it (via pop_vm_exec_apply as sysEXECUTE would),
            ;;; leaving the result(s) on the stack
            pop_vm_exec_apply(p)

        ;;; else pcr -- references non-local lvars
        ;;; or uninitialised lconstants, etc
        elseif flags &&/=_0 ALLOW_DELAYED_EVAL then
            ;;; Providing it doesn't use runtime-ident lvars, this will be
            ;;; evaluated after the whole outer procedure has been compiled
            ;;; (with 1 result assumed)
            p!PCR_FLAGS fi_|| M_PCR_EVALUATE -> p!PCR_FLAGS;
            p
        else
            mishap(0, 'ILLEGAL CODE FOR COMPILE-TIME EVALUATION')
        endunless
    endif;
    if return_result then result endif
enddefine;

    /*  Establish a new invocation of the VM compiler at execute level
    */
protected
define vars sysCOMPILE(compiler_p);
    lvars   pcr, procedure compiler_p;
    dlvars  restored_dlocals;
    dlocal  vm_current_pcr,
            vm_current_lblk,
            vm_code_end,
            vm_code_penult,
            vm_lex_assoc,
            vm_lex_push_counts  = [],
            vm_reprocess_instrs,
            vm_label_count,
            pop_new_lvar_list   = tmp_lvars,
            popexecute          = true,
            pop_vm_compiling_list= [],
            pop_vm_flags,
            pop_vm_dummy_idents = [],
            _vm_level           = _0,
            _vm_stack_count     = _0,
            _simstack           = _S_NONE,

            weakref pop_#_include_stack         = false,
            weakref $-Sys$-Prglst$- #_if_stack  = [],
        ;

    define lconstant do_dlocals(pcr, vm_dlocal_context, vm_dlocal_process);
        lvars id, vals, list, pcr, procedure accp;
        dlocal vm_dlocal_context, vm_dlocal_process;

        dlocal 0 %  if dlocal_context == 1 then
                        false -> pcr
                    elseif pcr then
                        do_dlocals(pcr, dlocal_context, dlocal_process)
                    endif,
                    if dlocal_context == 2 and pcr then
                        do_dlocals(pcr, dlocal_context, dlocal_process)
                    endif
                 %;

        if vm_dlocal_context fi_<= 2 then
            until (pcr!PCR_DLEXPR_LIST ->> list) == [] do
                dest_assoc(list) -> (id, vals, pcr!PCR_DLEXPR_LIST);
                fast_subscrv(1,vals) -> accp;   ;;; fast_idval, idval or fast_apply
                explode(vals) -> (, accp(id))
            enduntil
        elseif vm_dlocal_context == 3 then
            ;;; suspend
            until (pcr!PCR_DLEXPR_LIST ->> list) == [] do
                dest_assoc(list) -> (id, vals, pcr!PCR_DLEXPR_LIST);
                fast_subscrv(1,vals) -> accp;   ;;; fast_idval, idval or fast_apply
                accp, accp(id), explode(vals) -> (explode(vals), , accp(id));
                restored_dlocals -> fast_back(fast_back(list));
                list -> restored_dlocals
            enduntil
        else
            ;;; resume
            until (restored_dlocals ->> list) == [] do
                dest_assoc(list) -> (id, vals, restored_dlocals);
                fast_subscrv(1,vals) -> accp;   ;;; fast_idval, idval or fast_apply
                accp, accp(id), explode(vals) -> (explode(vals), , accp(id));
                pcr!PCR_DLEXPR_LIST -> fast_back(fast_back(list));
                list -> pcr!PCR_DLEXPR_LIST
            enduntil
        endif
    enddefine;

    dlocal 0 %  if dlocal_context == 1 then
                    false -> pcr, [] -> restored_dlocals
                elseif pcr then
                    do_dlocals(pcr, dlocal_context, dlocal_process)
                endif,
                if dlocal_context fi_<= 2 and pcr then
                    do_dlocals(pcr, dlocal_context, dlocal_process)
                endif
             %;

    Check_procedure(compiler_p);

    ;;; initialise top-level pcr
    Cons_pcr(false, false, 0, [], false) ->> pcr -> vm_current_pcr;
    Restore_current_pcr();

    if vm_pas_mode == "popc" then
        ;;; call through Popc procedure
        valof("popc_compstream_apply")(compiler_p)
    else
        compiler_p();
    endif;

    unless popexecute and vm_current_lblk!LBLK_LEVEL == 0 then
        mishap(0, 'sysCOMPILE: MISSING sysENDLBLOCK/sysENDPROCEDURE')
    endunless;

    Check_lconsts([])
enddefine;

    /*  Called by sys_pr_message
    */
define Pr_compiling_list(hdr_string);
    lvars pcr, hdr_string;
    if pop_vm_compiling_list /== [] then
        printf(hdr_string);
        fast_for pcr in pop_vm_compiling_list do
            spr(pcr!PD_PROPS)      ;;; pdprops of procedure being compiled
        endfor;
        cucharout(`\n`)
    endif
enddefine;

    /*  Run compile procedure with the option of executing
        code planted on return from procedure.
        If compile_p returns true, code is detached from current
        procedure and executed.
    */
protected
define vars sysEXEC_OPTION_COMPILE(compile_p);
    lvars id, lvar_env, compile_p,
        save_endcode = vm_code_end,
        save_new_lvar = pop_new_lvar_list,
        save_lvar_env = vm_current_lblk!LBLK_LVAR_ENV,
        save_lex_assoc = vm_lex_assoc
        ;
    dlocal pop_new_lvar_list;

    returnif(not(compile_p()) or pop_syntax_only) (false);

    ;;; Code planted to be detached and executed immediately
    if popexecute then
        []
    else
        ;;; use temp lvars declared since compile_p run
        [% until save_new_lvar == pop_new_lvar_list do
            dup(list_assoc_val(dest(save_new_lvar) -> save_new_lvar,
                                                    vm_lex_assoc))
        enduntil %]
    endif -> lvar_env;

    sysCOMPILE(
        vm_code_end, fast_back(save_endcode), lvar_env,
        procedure();
            lvars pcr = vm_current_pcr;
            -> pcr!PCR_LBLOCK!LBLK_LVAR_ENV
            -> fast_back(pcr!PCR_CODE_LIST) -> vm_code_end;
            sysEXECUTE()
        endprocedure);

    ;;; reset flags on token ids
    until lvar_env == [] do
        dest_assoc(lvar_env) -> (, id, lvar_env);
        _:M_LEX_USED -> id!ID_LEX_FLAGS
    enduntil;

    ;;; remove any temp lvars created since compile_p run
    save_lvar_env -> vm_current_lblk!LBLK_LVAR_ENV;
    save_lex_assoc -> vm_lex_assoc;
    [] -> fast_back(save_endcode);
    save_endcode -> vm_code_end;
    true
enddefine;

endsection;     /* $-Sys$-Vm */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 27 1996
        Changes to keep new label reference counts updated.
--- John Gibson, Mar 26 1996
        sysCOMPILE now deals with execute level dlocals (added to
        PCR_DLEXPR_LIST by sysLOCAL).
--- John Gibson, Jun 24 1995
        Changed sysEXEC_COMPILE to allow its 2nd arg to be integer flags,
        where 2:01 = old 'return result' and 2:10 means allow delayed
        evaluation for a (1-result) expression that uses non-local lvars etc.
--- John Gibson, Aug 16 1993
        Made pop_pas_mode an active variable assigning to internal var
        vm_pas_mode; now set to "popc" for Popc.
--- John Gibson, Jul  2 1993
        Moved initialisation of (set_)pop_pas_mode to initial.p
--- John Gibson, Oct 11 1992
        Put trap for POPC into sysCOMPILE
--- John Gibson, Feb  5 1991
        Added interrupt check in -Insert_nlgotos-
--- John Gibson, Dec 20 1990
        Corrected 2nd arg to call of -Get_goto_label_lblk- in
        -Pull_up_sym_labs- (so that backward jumps out of a local lblock
        plants a check).
--- John Gibson, Aug 14 1990
        Did the last change again, properly this time.
--- John Gibson, Jul 18 1990
        Ensured that pcr closures and lblock vectors are kept writeable after
        -sys_lock_system-.
--- John Gibson, Jul  7 1990
        Added -sysEXEC_COMPILE-.
--- John Gibson, Dec  4 1989
        Changes for new pop pointers
--- John Gibson, Aug  4 1989
        Introduced -pop_vm_dummy_idents- (used in -sys_current_ident-
        and -sys_use_current_ident-).
--- John Gibson, Jul  7 1989
        Added -pop_vm_exec_apply-
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, May  9 1989
        Introduced -pop_vm_compiling_list- and allowed -sysLBLOCK- to take
        a boolean argument saying whether lblock is executing/non-executing
        (i.e. whether -sysEXECUTE- can be called inside it).
--- John Gibson, May  4 1989
        Made -pop_vm_flags- be localised to every lblock.
--- John Gibson, Apr 30 1989
        Put into section $-Sys$-Vm.
--- John Gibson, Mar  1 1989
        Replaced -lmember- with -fast_lmember- in -Resolve_pcr-
--- John Gibson, Nov 23 1988
        Changes for lexical blocks.
--- John Gibson, Aug 17 1988
        Fixed problem in -Match_lvars-
--- John Gibson, Jul 26 1988
        Change to fields in PCR record.
--- John Gibson, Mar 27 1988
        -list_assoc- etc into section Sys
--- John Gibson, Mar 11 1988
        Name changed to vm_control.p (previously vmcontrol.p)
--- John Gibson, Mar  7 1988
        Couple of things into section Sys$-Prglst
 */
