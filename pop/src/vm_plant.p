/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/vm_plant.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF * VMCODE
 > Related Files:   SRC * VM_OPTIM.P, SRC * VM_ASM.P, SRC * VM_CONTROL.P
 */

;;; ------------ PLANTING VIRTUAL MACHINE INSTRUCTIONS -------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'vmdefs.ph'

constant
        procedure (maplist, appproperty, ident_declare, isundef,
        sys_current_ident, sys_use_current_ident),
        active (dlocal_context, dlocal_process)
    ;

vars
        popexecute, pop_vm_compiling_list
    ;

section $-Sys;

constant
        procedure (Idprops_flags, Check_token, Make_idval_undef,
        Nonactive_assign, Is_updateable, Sect$-Curr_pas_token)
    ;

endsection;

section $-Sys$-Vm;

constant
        procedure (I_CHECK, I_ERASE, I_SWAP, rI_MOVEI_LEX, rI_CHAINQ_LEX,
        rI_CONSTRUCT, Plant_call, SysCALLS, SysUCALLS, Plant_syspdr_call,
        Code_empty, Retrieve_pcr, Pcr_pdr_complete, Get_operand,
        New_save_lvar, Cons_lex_id, Passign_ident
        )
    ;

vars
        vm_dlocal_context, vm_dlocal_process
    ;

endsection;


;;; ----------------------------------------------------------------------

section $-Sys$-Vm =>
                    sysPUSHQ, sysPUSH, sysPOP, sysIDENT, sysPUSHS, sysERASE,
                    sysSWAP, sysCALLQ, sysUCALLQ, sysCALL, sysUCALL,
                    sysCALLS, sysUCALLS,
                    sysIFNOT, sysIFSO, sysAND, sysOR, sysGOTO, sysGO_ON,
                    sysLABEL, sysDLABEL, sysNEW_LABEL, sysLOCAL, sysSYNTAX,
                    sysVARS, sysCONSTANT, sysNEW_LVAR, sysLVARS, sysDLVARS,
                    sysLCONSTANT, sysPASSIGN, sysUPASSIGN,
                    sysCONSTRUCT
                    ;

vars
    _simstack       = _S_NONE,
    _vm_stack_count = _0
    ;

lvars
    compiling_top_dlexpr = false,
    ;

lconstant
    inval_idstring = 'vm-ident:name-ref-inval',
    ambig_idstring = 'vm-ident-lex:name-decl-ambig',
    ;


define Plant();
    lvars instr = Cons_inst(), code_end = vm_code_end;
    if fast_front(code_end) then
        ;;; last not unplanted
        code_end -> vm_code_penult;     ;;; shuffle current end to penultimate
        conspair(instr, []) ->> fast_back(code_end) -> vm_code_end;
    else
        ;;; last unplanted
        instr -> fast_front(code_end)
    endif
enddefine;

define Unplant(_popstack);
    lvars code_end = vm_code_end, repinst = vm_reprocess_instrs, _popstack;
    if repinst /== [] and fast_front(repinst) == code_end then
        ;;; remove from reprocess instruction list
        fast_back(repinst) -> vm_reprocess_instrs
    endif;
    if vm_code_penult then
        _free_pairs -> fast_back(code_end);
        code_end -> _free_pairs;
        vm_code_penult -> vm_code_end;
        [] -> fast_back(vm_code_end);
        false -> vm_code_penult;
        if _popstack then
            ;;; don't clear a previous XPUSH
            _shift(_simstack, _S_POP1) -> _simstack
        endif
    else
        false -> fast_front(code_end);
        if _popstack then _POP_simstack -> _simstack endif
    endif
enddefine;

define lconstant Plant_reprocess_next();
    lvars code_end = vm_code_end;
    if fast_front(code_end) then
        ;;; not unplanted -- cons new pair for next reprocess instr
        code_end -> vm_code_penult;     ;;; shuffle current end to penultimate
        conspair(false, []) ->> fast_back(code_end) ->> code_end
                                    -> vm_code_end;
    endif;
    ;;; remember codelist pair for reprocess instruction
    code_end :: vm_reprocess_instrs -> vm_reprocess_instrs
enddefine;

define lconstant Plant_check();
    unless vm_pas_mode then Plant(I_CHECK, 1) endunless;
    _S_NONE -> _simstack;
    _0 -> _vm_stack_count
enddefine;

define lconstant Get_symbol_label(slabel, lblk) -> labpair;
    lvars labpair, slabel, lblk, slabels = lblk!LBLK_SYMBOL_LABELS;
    unless list_assoc_val(slabel, slabels) ->> labpair then
        conspair(M_LAB_INITIAL, slabel) -> labpair;
        slabel :: (labpair :: slabels) -> lblk!LBLK_SYMBOL_LABELS
    endunless
enddefine;

define Get_goto_label_lblk(label, check_back, lblk) -> labpair;
    lvars labpair = label, label, check_back, op, lblk, _flags;
    unless ispair(label) then
        Get_symbol_label(label, lblk) -> labpair
    endunless;
    _int(fast_front(labpair)) -> _flags;
    unless _flags _bitst (_:M_LAB_PLANTED _biset _:M_LAB_REFCOUNT) then
        ;;; label not seen before -- count it
        _vm_level _biset _flags -> _flags;      ;;; mark with current level
        vm_label_count fi_+ 1 -> vm_label_count
    elseif _flags _bimask _:M_LAB_LEVEL _lt _vm_level then
        ;;; gone to or planted at an outer level -- must be non-local
        ;;; add 1 to label count so this comes out non-zero at the end
        vm_label_count fi_+ 1 -> vm_label_count
    endunless;
    if  check_back and _flags _bitst _:M_LAB_PLANTED
    and not(_flags _bitst _:M_LAB_HAS_CHECK)
    and _flags _bimask _:M_LAB_LEVEL == _vm_level
    and (pop_debugging or pop_vm_flags &&=_0 VM_NO_BACK_JUMP_CHECKS)
    and (not(vm_pas_mode) or vm_pas_mode == "popc")
    then
        ;;; local backward jump -- plant check instruction after label
        _flags _biset _:M_LAB_HAS_CHECK -> _flags;
        if vm_pas_mode == "popc" then "pas_CHECK" else I_CHECK endif -> op;
        if labpair == vm_code_end then
            Plant(op, 1)
        else
            conspair(Cons_inst(op, 1), fast_back(labpair))
                                        ->> fast_back(labpair) -> op;
            if labpair == vm_code_penult then op -> vm_code_penult endif
        endif
    endif;
    ;;; increment reference count for label
    unless _flags _bitst _:M_LAB_REFMAX then
        _flags _add _:M_LAB_REF1 -> _flags
    endunless;
    _pint(_flags) -> fast_front(labpair)
enddefine;

define Get_goto_label() with_nargs 2;
    chain(vm_current_lblk, Get_goto_label_lblk)
enddefine;

define Assign_pas_token(id, token);
    lvars id, token, p = valof("pas_ident_token");
    if isprocedure(p) and not(p(id)) then
        $-Sys$-Sect$-Curr_pas_token(token) -> p(id)
    endif
enddefine;

define lconstant Add_unresolved(item);
    lvars   lblk1 = vm_current_lblk, lblk2, item, l,
            ur_list = vm_current_pcr!PCR_UNRESOLVED_LIST, _diff
        ;
    unless list_assoc(item, ur_list) ->> l then
        cons_assoc(item, lblk1, ur_list) -> vm_current_pcr!PCR_UNRESOLVED_LIST;
        return
    endunless;

    ;;; get common ancestor lblock
    fast_front(l) -> lblk2;
    lblk1!LBLK_LEVEL fi_- lblk2!LBLK_LEVEL -> _diff;
    while _diff fi_< 0 do
        lblk2!LBLK_OUTER_BLOCK -> lblk2, _diff fi_+ 1 -> _diff
    endwhile;
    while _diff fi_> 0 do
        lblk1!LBLK_OUTER_BLOCK -> lblk1, _diff fi_- 1 -> _diff
    endwhile;
    until lblk1 == lblk2 do
        lblk1!LBLK_OUTER_BLOCK -> lblk1;
        lblk2!LBLK_OUTER_BLOCK -> lblk2
    enduntil;
    lblk1 -> fast_front(l)
enddefine;

define Push_lex(item);
    lvars list, item, _incr = 1;
    unless popexecute then
        if pop_vm_flags &&/=_0 VM_DISCOUNT_LEX_PROC_PUSHES then
            ;;; don't increase count, but it must be added to
            ;;; vm_lex_push_counts
            0 -> _incr
        endif;
        if list_assoc(item, vm_lex_push_counts) ->> list then
            fast_front(list) fi_+ _incr -> fast_front(list)
        else
            item :: (_incr :: vm_lex_push_counts) -> vm_lex_push_counts
        endif
    endunless;
    Add_unresolved(item)
enddefine;

define Unpush_lex(item);
    lvars list, item;
    if list_assoc(item, vm_lex_push_counts) ->> list then
        fast_front(list) fi_- 1 -> fast_front(list)
    endif
enddefine;

define Action_lex_pushes();
    lvars   item, id, assoc = vm_lex_push_counts, pcr, upd,
            nloc_env = vm_current_pcr!PCR_LBLOCK!LBLK_LVAR_ENV,
            _flags, _newflags, _count
        ;
    until assoc == [] do
        dest_assoc(assoc) -> assoc -> _count -> item;
        ;;; has been pushed
        if item!KEY == ident_key then
            ;;; lconstant ident
            if _count == 0 then
                _:M_LEX_PUSHED
            else
                _:M_LEX_PUSHED _biset _:M_LEX_COUNTED_PUSH
            endif -> _newflags
        else
            ;;; pcr
            item -> pcr;
            nextunless(pcr!PCR_CLOS_LVAR ->> item);
            if _count /== 0 then
                ;;; had a counted push -- set M_PCR_PUSHED
                pcr!PCR_FLAGS fi_|| M_PCR_PUSHED -> pcr!PCR_FLAGS;
                if pcr!PD_UPDATER ->> upd then
                    upd!PCR_FLAGS fi_|| M_PCR_PUSHED -> upd!PCR_FLAGS
                endif
            endif;
            ;;; allow the local id for the pcr to be marked used
            ;;; (may not actually be used)
            _:M_LEX_USED -> _newflags
        endif;

        ;;; lconstant or pcr clos identifier
        ;;; mark the ids with _newflags all the way up
        item -> id;
        if list_assoc_val(id, nloc_env) ->> item then
            item -> id
        endif;
        repeat
            id!ID_LEX_FLAGS -> _flags;
            _flags _biset _newflags -> id!ID_LEX_FLAGS;
            quitunless(_flags _bitst _:M_LEX_NON_LOCAL);
            fast_idval(id) -> id
        endrepeat
    enduntil;
    [] -> vm_lex_push_counts
enddefine;

protected
define vars sysPUSHQ(item);
    lvars item, list, _type;
    returnif(pop_syntax_only);

    _S_ANY _biset _S_XPUSH -> _type;
    if isprocedure(item) then
        ;;; procedure
        if Is_pcr(item) then
            unless item!PCR_FLAGS &&/=_0 M_PCR_EVALUATE then
                _S_PDR _biset _S_XPUSH -> _type
            endunless;
            Push_lex(item);
            Plant_reprocess_next();
            rI_MOVEQ_LEX
        else
            _S_PDR _biset _S_XPUSH -> _type;
            Moveq_op(item)
        endif
    else
        Moveq_op(item)
    endif;
    Plant((), item, 2);

    unless vm_pas_mode then
        _PUSH_simstack _biset _type -> _simstack;

        ;;; crude check for stack overflow
        if (_vm_stack_count _add _1 ->> _vm_stack_count) _sgr _32 then
            ;;; 32 or more consecutive pushes -- insert stack check
            Plant_check()
        endif
    endunless
enddefine;
;;;
constant procedure SysPUSHQ = sysPUSHQ;


define Plant_pop(id);
    lvars instr = fast_front(vm_code_end), id;
    if _simstack _bitst _S_XPUSH then
        ;;; produce 2 arg I_MOVE/Q instruction etc
        Cons_inst(instr!INST_OP, instr!INST_ARGS[_0], id, 3)
                                            -> fast_front(vm_code_end);
        Garbage_inst(instr)     ;;; garbage old instr
    elseif isvector(instr) and instr!INST_OP == I_MOVES then
        Cons_inst(I_STORE, id, 2) -> fast_front(vm_code_end)
    else
        Plant(I_POP, id, 2)
    endif;
    _POP_simstack -> _simstack
enddefine;

define lconstant Checkr_assign(_idprops);
    lvars _idprops;
    returnunless(_idprops _bitst _:M_ID_CONST_OR_PROT) (false);
    if _idprops _bitst _:M_ID_LEX then
        return('LCONSTANT')
    elseif pop_vm_flags &&=_0 VM_NOPROT_PVARS then
        if _idprops _bitst _:M_ID_PROTECT then
            return('PROTECTED IDENTIFIER')
        elseif pop_vm_compiling_list /== [] then
            return('CONSTANT')
        endif
    endif;
    false
enddefine;

define lconstant Checkr_pdrpop(/*item,*/ _word);
    lvars _word;
    unless isprocedure(dup()) then
        mishap((), _word, 2, 'ASSIGNING NON-PROCEDURE TO PROCEDURE IDENTIFIER')
    endunless
enddefine;

define :inline lconstant USE_CONST_VAL(id, _idprops);
    (_idprops _bitst _:M_ID_ASSIGNED_CONST
     and (_idprops _bitst _:M_ID_LEX or id <@(w) _system_end
          or pop_debugging /== true))
enddefine;

protected
define vars sysPUSH(/*word*/) with_nargs 1;
    lvars id, instr, _idprops, _type, _nonactive;
    sys_use_current_ident(/*word*/, true) -> (id, _nonactive);
    id!ID_IDENTPROPS -> _idprops;

    if _idprops _bitst _:M_ID_ACTIVE and not(_nonactive) then
        Plant_call(id);
        return
    endif;

    returnif(pop_syntax_only);

    if vm_pas_mode then
        if _idprops _bitst _:M_ID_LEX and _idprops _bitst _:M_ID_CONSTANT then
            ;;; lconstant
            if _idprops _bitst _:M_ID_ASSIGNED_CONST then
                ;;; initialised
                SysPUSHQ(fast_idval(id));
            else
                ;;; undef lconstant
                Push_lex(id);
                Plant_reprocess_next();
                Plant(rI_MOVEQ_LEX, id, 2)
            endif
        else
            Plant("pas_PUSH", id, 2);
        endif;
        return
    endif;

    if USE_CONST_VAL(id, _idprops) then
        SysPUSHQ(fast_idval(id));
        return
    endif;
    _S_ANY -> _type;
    if _idprops _bitst _:M_ID_PROCEDURE_VAL then _S_PDR -> _type endif;
    fast_front(vm_code_end) -> instr;
    if isvector(instr) and instr!INST_OP == I_POP and instr!INST_ARGS[_0] == id
    then
        I_STORE -> instr!INST_OP;
        _PUSH_simstack _biset _type -> _simstack;
    else
        if _idprops _bitst _:M_ID_CONSTANT and _idprops _bitst _:M_ID_LEX then
            ;;; undef lconstant
            Push_lex(id);
            Plant_reprocess_next();
            rI_MOVEQ_LEX
        else
            I_MOVE
        endif;
        Plant(id, 2);
        _PUSH_simstack _biset _type _biset _S_XPUSH -> _simstack;
    endif;

    ;;; crude check for stack overflow
    if (_vm_stack_count _add _1 ->> _vm_stack_count) _sgr _32 then
        ;;; 32 or more consecutive pushes -- insert stack check
        Plant_check()
    endif
enddefine;
;;;
define updaterof sysPUSH(/*word*/) with_nargs 1;
    chain(/*word*/, sysPOP)
enddefine;
;;;
constant procedure SysPUSH = sysPUSH;


protected
define vars sysPOP(word);
    lvars id, word, _idprops, _pchk, _ms, _nonactive;

    define lconstant Set_const(/* newval, */ id, token) with_nargs 3;
        lvars id, token, _idprops;
        if (id!ID_IDENTPROPS ->> _idprops) _bitst _:M_ID_ASSIGNED_CONST then
            mishap((), token, 2, 'ILLEGAL ASSIGNMENT TO CONSTANT IDENTIFIER');
        else
            () -> fast_idval(id);
            unless id!ID_PERM_FLAGS _bitst _:M_PERM_NOT_DEF then
                _idprops _biset _:M_ID_ASSIGNED_CONST -> id!ID_IDENTPROPS
            endunless
        endif
    enddefine;

    sys_use_current_ident(word) -> (id, _nonactive);

    id!ID_IDENTPROPS -> _idprops;
    if _idprops _bitst _:M_ID_ACTIVE and not(_nonactive) then
        -> Plant_call(id);
        return
    endif;

    if Checkr_assign(_idprops) ->> _ms then
        mishap(word, 1, 'COMPILING ASSIGNMENT TO ' <> _ms, inval_idstring)
    endif;

    returnif(pop_syntax_only);

    _idprops _bitst _:M_ID_PROCEDURE_VAL
    and (pop_debugging or pop_vm_flags &&=_0 VM_NO_TYPED_VAR_CHECKS) -> _pchk;

    if vm_pas_mode then
        if _pchk and vm_pas_mode == "popc" then
            Plant("pas_TYPE_CHECK", procedure_key, 2)
        endif;
        Plant("pas_POP", id, 2);
        return
    endif;

    if _pchk and _simstack _bimask _S_TYPE /== _S_PDR then
        Plant(I_MOVEQ, word, 2);
        Plant(I_CALLABS, Checkr_pdrpop, 2);
        _simstack _biclear _S_XPUSH -> _simstack
    endif;
    if _idprops _bitst _:M_ID_CONSTANT then
        Plant(I_MOVEQ, id, 2);
        Plant(I_MOVEQ, word, 2);
        Plant(I_CALLABS, Set_const, 2);
        _S_NONE -> _simstack
    else
        Plant_pop(id);
        if _vm_stack_count _sgr _0 then
            _vm_stack_count _sub _1 -> _vm_stack_count
        endif
    endif
enddefine;
;;;
constant procedure SysPOP = sysPOP;

protected
define vars sysIDENT(word);
    lvars id, word, item, _idprops, _flags;
    sys_use_current_ident(word, true) -> (id, );
    returnif(pop_syntax_only);
    id!ID_IDENTPROPS -> _idprops;

    if _idprops _bitst _:M_ID_CONSTANT and _idprops _bitst _:M_ID_LEX then
        ;;; lconstant
        if _idprops _bitst _:M_ID_ASSIGNED_CONST
        and Is_pcr(fast_idval(id) ->> item) then
            Push_lex(item)
        endif;
        Push_lex(id);
        Plant_reprocess_next();
        rI_MOVEI_LEX
    elseif vm_pas_mode then
        "pas_IDENT"
    else
        I_MOVEQ
    endif;
    Plant((), id, 2);

    if _idprops _bitst _:M_ID_LEX_TOKEN then
        ;;; follow the chain to the home identifier
        repeat
            id!ID_LEX_FLAGS -> _flags;
            quitunless(_flags _bitst _:M_LEX_NON_LOCAL);
            fast_idval(id) -> id
        endrepeat;
        ;;; mark home id used non-locally/rtid access
        unless _flags _bitst _:M_LEX_USE_DYNAMIC
        or pop_vm_flags &&/=_0 VM_DISCOUNT_LEX_PROC_PUSHES then
            _flags _biset _:M_LEX_RTID_ACCESS -> _flags
        endunless;
        _flags _biset _:M_LEX_USED_NON_LOCALLY -> id!ID_LEX_FLAGS
    endif;

    _PUSH_simstack _biset (_S_ANY _biset _S_XPUSH) -> _simstack;

    ;;; crude check for stack overflow
    if (_vm_stack_count _add _1 ->> _vm_stack_count) _sgr _32 then
        ;;; 32 or more consecutive pushes -- insert stack check
        Plant_check()
    endif
enddefine;

define SysPUSH_OR_IDENT(word);
    lvars   (id, _nonactive) = sys_use_current_ident(word, true), word,
            _idprops = id!ID_IDENTPROPS;

    returnif(pop_syntax_only);

    if not(_idprops _bitst _:M_ID_ACTIVE and not(_nonactive))
    and (USE_CONST_VAL(id, _idprops)
         or (_idprops _bitst _:M_ID_CONSTANT
             and (_idprops _bitst _:M_ID_LEX or vm_pas_mode == "popc")))
    then
        sysPUSH
    else
        sysIDENT
    endif (word)
enddefine;

protected
define vars sysPUSHS(dummy);        ;;; has a dummy argument
    lvars dummy, _type;
    returnif(pop_syntax_only);
    if vm_pas_mode then
        Plant("pas_PUSHS", 1)
    else
        _simstack _bimask _S_TYPE -> _type;
        if _zero(_type) then _S_ANY -> _type endif;
        _PUSH_simstack _biset _type -> _simstack;
        Plant(I_MOVES, 1);

        ;;; crude check for stack overflow
        if (_vm_stack_count _add _1 ->> _vm_stack_count) _sgr _32 then
            ;;; 32 or more consecutive pushes -- insert stack check
            Plant_check()
        endif;
    endif
enddefine;
lconstant procedure SysPUSHS = sysPUSHS;

protected
define vars sysERASE(dummy);        ;;; has a dummy argument
    lvars dummy;
    returnif(pop_syntax_only);
    if vm_pas_mode then
        Plant("pas_ERASE", 1)
    else
        if _simstack _bitst _S_XPUSH then
            Unplant(true)       ;;; unplant the push(q)
        else
            Plant(I_ERASE, 1);
            _POP_simstack -> _simstack
        endif
    endif
enddefine;
;;;
lconstant procedure SysERASE = sysERASE;

protected
define vars sysSWAP(i, j);
    lvars i, j, _ival, _jval, _si, _sj;
    returnif(pop_syntax_only);
    Check_integer(i, 1);
    Check_integer(j, 1);
    if i == j then return endif;
    if vm_pas_mode then
        Plant("pas_SWAP", i, j, 3);
        return
    endif;
    i fi_- 1 -> i;
    j fi_- 1 -> j;
    Plant(I_SWAP, i, j, 3);
;;; Clear XPUSH
    _REP_ANY_simstack -> _simstack;
;;; Now swap _simstack entries around ...
    _int(i) _mult _S_PUSH -> _si;
    _int(j) _mult _S_PUSH -> _sj;
    if _zero(_shift(_simstack, _negate(_si)) _bimask _S_TYPE ->> _ival) then
        _S_ANY -> _ival
    endif;
    if _zero(_shift(_simstack, _negate(_sj)) _bimask _S_TYPE ->> _jval) then
        _S_ANY -> _jval
    endif;
    _simstack _biclear _shift(_S_TYPE _biset _S_XPUSH, _si)
        _biset _shift(_jval, _si) -> _simstack;
    _simstack _biclear _shift(_S_TYPE _biset _S_XPUSH, _sj)
        _biset _shift(_ival, _sj) -> _simstack;
    _simstack _bimask _S_CLEAR_OVERFLOW -> _simstack;
enddefine;

protected
define vars sysCALLQ(item);
    lvars item, pdr, _is_pcr;
    unless iscompound(item) then
        mishap(item, 1, 'COMPILING CALL TO NON-STRUCTURE')
    endunless;
    returnif(pop_syntax_only);
    if item == nonactive _ then
        ;;; value of anonymous var
        chain("pop_undef", SysPUSH)
    endif;
    _0 -> _vm_stack_count;
    if (Is_pcr(item) ->> _is_pcr) and (Pcr_pdr_complete(item) ->> pdr) then
        ;;; OK not to bother with rI_CALLQ_LEX
        false -> _is_pcr;
        pdr -> item
    endif;
    if _is_pcr then
        Add_unresolved(item);
        Plant_reprocess_next();
        rI_CALLQ_LEX
    elseif compiling_top_dlexpr
    and (item == nonactive dlocal_context or item == nonactive dlocal_process)
    then
        if item == nonactive dlocal_context then ident vm_dlocal_context
        else ident vm_dlocal_process
        endif -> item;
        if vm_pas_mode then "pas_PUSH" else I_MOVE endif
    elseif vm_pas_mode then
        "pas_CALLQ"
    elseif isprocedure(item) then
        if item <@(w) _system_end then
            ;;; system procedure
            if item!PD_FLAGS _bitst _:M_PD_DOES_PLOG_PUSH then
                ;;; change the check subroutine to one that also
                ;;; checks for plog trail overflow
                vm_current_pcr!PCR_FLAGS fi_|| M_PCR_PLOG_TRAIL_CHECK
                                    -> vm_current_pcr!PCR_FLAGS
            endif;
            Plant_syspdr_call(item);            ;;; plants code
            return
        else
            I_CALLPQ
        endif;
    else
        I_CALLQ
    endif;
    Plant(item, 2);
    _S_NONE -> _simstack
enddefine;
;;;
define updaterof sysCALLQ(/*item*/) with_nargs 1;
    chain(/*item*/, sysUCALLQ)
enddefine;
;;;
constant procedure SysCALLQ = sysCALLQ;

protected
define vars sysUCALLQ(item);
    lvars item;
    unless iscompound(item) then
        mishap(item, 1, 'COMPILING UPDATER CALL TO NON-STRUCTURE')
    endunless;
    returnif(pop_syntax_only);
    if item == nonactive _ then
        ;;; assigning to anonymous var
        chain(0, SysERASE)
    endif;
    _0 -> _vm_stack_count;
    if Is_pcr(item) then
        Add_unresolved(item);
        Plant_reprocess_next();
        rI_UCALLQ_LEX
    elseif vm_pas_mode then
        "pas_UCALLQ"
    elseif isprocedure(item) then
        if Is_updateable(item) then
            I_UCALLPQ
        elseunless item!PD_UPDATER then
            mishap(item, 1, 'COMPILING CALL TO NON-EXISTENT UPDATER')
        else
            Plant_syspdr_call(item!PD_UPDATER);     ;;; plants code
            return
        endif
    else
        I_UCALLQ
    endif;
    Plant(item, 2);
    _S_NONE -> _simstack
enddefine;
;;;
constant procedure SysUCALLQ = sysUCALLQ;

define Plant_call(id);
    lvars id, instr, _idprops = id!ID_IDENTPROPS;

    returnif(pop_syntax_only);

    if vm_pas_mode
    and id /== ident _
    and not(compiling_top_dlexpr
            and (id == ident dlocal_context or id == ident dlocal_process))
    then
        if _idprops _bitst _:M_ID_LEX and _idprops _bitst _:M_ID_CONSTANT then
            ;;; lconstant
            if _idprops _bitst _:M_ID_ASSIGNED_CONST then
                ;;; initialised
                SysCALLQ(fast_idval(id))
            else
                ;;; undef lconstant
                Add_unresolved(id);
                Plant_reprocess_next();
                Plant(rI_CALLQ_LEX, id, 2)
            endif
        elseif isvector(fast_front(vm_code_end) ->> instr)
        and instr!INST_OP == rI_MOVEQ_LEX then
            if id == ident chain then
                rI_CHAINQ_LEX -> instr!INST_OP
            else
                if fast_lmember(id, #_< [%ident applist, ident maplist,
                                     ident appdata, ident appproperty%] >_# )
                then
                    Unpush_lex(instr!INST_ARGS[_0])     ;;; 'undo' the lex push
                endif;
                Plant("pas_CALL", id, 2)
            endif
        else
            Plant("pas_CALL", id, 2)
        endif;
        return
    endif;

    if USE_CONST_VAL(id, _idprops) then
        SysCALLQ(fast_idval(id));
        return
    elseif _idprops _bitst _:M_ID_CONSTANT and _idprops _bitst _:M_ID_LEX then
        ;;; undef lconstant
        Add_unresolved(id);
        Plant_reprocess_next();
        rI_CALLQ_LEX
    elseif _idprops _bitst _:M_ID_PROCEDURE_VAL then
        I_CALLP
    else
        I_CALL
    endif;
    Plant(id, 2);
    _S_NONE -> _simstack;
    _0 -> _vm_stack_count
enddefine;
;;;
define updaterof Plant_call(id);
    lvars id, val, _idprops = id!ID_IDENTPROPS;

    returnif(pop_syntax_only);
    if _idprops _bitst _:M_ID_LEX and _idprops _bitst _:M_ID_ASSIGNED_CONST
    and isprocedure(fast_idval(id) ->> val) and val!PD_UPDATER then
        ;;; lconstant initialised to procedure with updater -- assume constant
        SysCALLQ(val!PD_UPDATER);
        return
    endif;

    if vm_pas_mode and id /== ident _ then
        if _idprops _bitst _:M_ID_LEX and _idprops _bitst _:M_ID_CONSTANT then
            ;;; lconstant
            if _idprops _bitst _:M_ID_ASSIGNED_CONST then
                ;;; initialised
                SysUCALLQ(fast_idval(id))
            else
                ;;; undef lconstant
                Add_unresolved(id);
                Plant_reprocess_next();
                Plant(rI_UCALLQ_LEX, id, 2)
            endif
        else
            Plant("pas_UCALL", id, 2)
        endif;
        return
    endif;

    if USE_CONST_VAL(id, _idprops) then
        SysUCALLQ(fast_idval(id));
        return
    elseif _idprops _bitst _:M_ID_CONSTANT and _idprops _bitst _:M_ID_LEX then
        ;;; undef lconstant
        Add_unresolved(id);
        Plant_reprocess_next();
        rI_UCALLQ_LEX
    elseif _idprops _bitst _:M_ID_PROCEDURE_VAL then
        I_UCALLP
    else
        I_UCALL
    endif;
    Plant(id, 2);
    _S_NONE -> _simstack;
    _0 -> _vm_stack_count
enddefine;

protected
define vars sysCALL(/*word*/) with_nargs 1;
    lvars id, _nonactive;
    sys_use_current_ident(/*word*/, true) -> (id, _nonactive);
    if id!ID_IDENTPROPS _bitst _:M_ID_ACTIVE and not(_nonactive) then
        Plant_call(id);
        SysCALLS(0)
    else
        Plant_call(id)
    endif
enddefine;
;;;
define updaterof sysCALL(/*word*/) with_nargs 1;
    chain(/*word*/, sysUCALL)
enddefine;
;;;
constant procedure SysCALL = sysCALL;

protected
define vars sysUCALL(/*word*/) with_nargs 1;
    lvars id, _nonactive;
    sys_use_current_ident(/*word*/, true) -> (id, _nonactive);
    if id!ID_IDENTPROPS _bitst _:M_ID_ACTIVE and not(_nonactive) then
        Plant_call(id);
        SysUCALLS(0)
    else
        -> Plant_call(id)
    endif
enddefine;
;;;
constant procedure SysUCALL = sysUCALL;

protected
define vars sysCALLS(item);         ;;; has a dummy argument
    lvars instr, opcode, item;
    returnif(pop_syntax_only);
    fast_front(vm_code_end) -> instr;

    if vm_pas_mode then
        if isvector(instr) and instr!INST_OP == rI_MOVEQ_LEX then
            Unpush_lex(instr!INST_ARGS[_0]);    ;;; 'undo' the lex push
            rI_CALLQ_LEX -> instr!INST_OP
        else
            Plant("pas_CALLS", 1)
        endif;
        return
    endif;

    _0 -> _vm_stack_count;
    if _simstack _bitst _S_XPUSH then
        instr!INST_OP -> opcode;
        instr!INST_ARGS[_0] -> item;
        if opcode == I_MOVE then
            if item!ID_IDENTPROPS _bitst _:M_ID_PROCEDURE_VAL then I_CALLP
            else I_CALL
            endif -> instr!INST_OP
        else
            if opcode == rI_MOVEQ_LEX then
                Unpush_lex(item);                   ;;; 'undo' the lex push
                rI_CALLQ_LEX -> instr!INST_OP
            else
                Unplant(true);                      ;;; unplant the push
                SysCALLQ(item);
                return
            endif
        endif
    else
        Plant(I_CALLS, 1)
    endif;
    _S_NONE -> _simstack
enddefine;
;;;
define updaterof sysCALLS(/*item*/) with_nargs 1;
    chain(/*item*/, sysUCALLS)
enddefine;
;;;
constant procedure SysCALLS = sysCALLS;

protected
define vars sysUCALLS(item);
    lvars instr, opcode, item;
    returnif(pop_syntax_only);
    fast_front(vm_code_end) -> instr;

    if vm_pas_mode then
        if isvector(instr) and instr!INST_OP == rI_MOVEQ_LEX then
            Unpush_lex(instr!INST_ARGS[_0]);    ;;; 'undo' the lex push
            rI_UCALLQ_LEX -> instr!INST_OP
        else
            Plant("pas_UCALLS", 1)
        endif;
        return
    endif;

    _0 -> _vm_stack_count;
    if _simstack _bitst _S_XPUSH then
        fast_front(vm_code_end) -> instr;
        instr!INST_OP -> opcode;
        instr!INST_ARGS[_0] -> item;
        if opcode == I_MOVE then
            if item!ID_IDENTPROPS _bitst _:M_ID_PROCEDURE_VAL then I_UCALLP
            else I_UCALL
            endif -> instr!INST_OP
        else
            if opcode == rI_MOVEQ_LEX then
                Unpush_lex(item);                   ;;; 'undo' the lex push
                rI_UCALLQ_LEX -> instr!INST_OP
            else
                Unplant(true);                      ;;; unplant the push
                SysUCALLQ(item);
                return
            endif
        endif
    else
        Plant(I_UCALLS, 1)
    endif;
    _S_NONE -> _simstack
enddefine;
;;;
constant procedure SysUCALLS = sysUCALLS;


define lconstant Plant_condition(label, testval, I_op, pas_op);
    lvars instr, label, testval, I_op, pas_op;
    returnif(pop_syntax_only);
    if vm_pas_mode then
        Plant(pas_op, Get_goto_label(label, true), testval, 3);
        return
    endif;

    if isvector(fast_front(vm_code_end) ->> instr) and instr!INST_OP == I_NOT
    then
        ;;; pick up a preceding I_NOT
        if I_op == I_BOOL then
            ;;; I_NBOOL is I_NOT followed by I_BOOL
            I_NBOOL -> I_op
        else
            ;;; reverse test for I_IF
            not(testval) -> testval
        endif;
        Unplant(false);         ;;; remove I_NOT
        if isvector(fast_front(vm_code_end) ->> instr)
        and instr!INST_OP == I_MOVE and instr!V_LENGTH == _2 then
            _simstack _biset _S_XPUSH -> _simstack
        endif
    endif;
    if Get_operand() ->> instr then Unplant(false) endif;
    Plant(I_op, Get_goto_label(label, true), testval, I_BRCOND, instr, 5);
    _POP_simstack -> _simstack
enddefine;

protected
define vars sysIFNOT =
    Plant_condition(%false, I_IF,   "pas_IF"%)
enddefine;

protected
define vars sysIFSO =
    Plant_condition(%true,  I_IF,   "pas_IF"%)
enddefine;
lconstant procedure SysIFSO = sysIFSO;

protected
define vars sysAND =
    Plant_condition(%false, I_BOOL, "pas_BOOL"%)
enddefine;

protected
define vars sysOR   =
    Plant_condition(%true,  I_BOOL, "pas_BOOL"%)
enddefine;


protected
define vars sysGOTO(label);
    lvars label;
    returnif(pop_syntax_only);
    Get_goto_label(label, true) -> label;
    if vm_pas_mode then
        Plant("pas_GOTO", label, 2)
    else
        Plant(I_GOTO, label, I_BR, 3)
    endif;
    _S_NONE -> _simstack
enddefine;

define Plant_label(label, _is_dlabel);
    lvars labelpair, label, _flags, _is_dlabel;
    returnif(pop_syntax_only);
    unless ispair(label ->> labelpair) then
        Get_symbol_label(label, vm_current_lblk) -> labelpair
    endunless;
    _int(fast_front(labelpair)) -> _flags;      ;;; label code
    if _flags _bitst _:M_LAB_PLANTED then
        mishap(if ispair(label) then 'system label' else label endif,
                            1, 'DUPLICATE LABEL', 'vm-label:name-decl-ambig')
    elseif _flags _bitst _:M_LAB_REFCOUNT then
        if _flags _bimask _:M_LAB_LEVEL _lt _vm_level then
            ;;; there was an outer goto to this inner procedure
            ;;; (can't happen with symbolic labels because of scoping rules)
            mishap(0, 'ILLEGAL GOTO, ETC DETECTED')
        endif;
        ;;; count off labels previously gone to
        vm_label_count fi_- 1 -> vm_label_count
    endif;
    if _is_dlabel then _flags _biset _:M_LAB_DLABEL -> _flags endif;
    _pint(_flags _biclear _:M_LAB_LEVEL _biset _vm_level
                        _biset _:M_LAB_PLANTED) -> fast_front(labelpair);
    [] -> fast_back(labelpair);
    vm_code_end -> vm_code_penult;
    labelpair ->> fast_back(vm_code_end) -> vm_code_end;
    _S_NONE -> _simstack
enddefine;

protected
define vars sysLABEL    = Plant_label(%false%) enddefine;
lconstant procedure SysLABEL = sysLABEL;

protected
define vars sysDLABEL   = Plant_label(%true%)  enddefine;

protected
define vars sysNEW_LABEL= conspair(%M_LAB_INITIAL, 'system label'%) enddefine;
lconstant procedure SysNEW_LABEL = sysNEW_LABEL;


protected
define vars sysGO_ON(/*lablist,*/ elselab);
    lvars lab, lablist, elselab, go_instr, base, _ichk;
    if isprocedure(elselab) then
        ;;; code-planting procedure which will return (lablist, elselab, base)
        if pop_syntax_only then
            fast_apply(elselab) -> (, , );
            return
        endif
    else
        if isinteger(elselab) then
            (), elselab -> (elselab, base)
        else
            1 -> base
        endif -> lablist;
        returnif(pop_syntax_only)
    endif;

    pop_debugging or pop_vm_flags &&=_0 VM_NO_CHECK_GO_ON_INT -> _ichk;
    if vm_pas_mode then
        if _ichk and vm_pas_mode == "popc" then
            Plant("pas_TYPE_CHECK", integer_key, 2)
        endif;
        "pas_GO_ON"
    else
        if _ichk then
            SysCALL(SysPUSHS(0), "isinteger");
            SysIFSO(SysNEW_LABEL() ->> lab);
            Plant(I_CALLABS, Inline_checkr_integer, 2);
            SysLABEL(lab)
        endif;
        I_GO_ON
    endif;
    Plant((), 0, 0, 0, 4);      ;;; the GO_ON
    _S_NONE -> _simstack;
    fast_front(vm_code_end) -> go_instr;

    if isprocedure(elselab) then
        fast_apply(elselab) -> (lablist, elselab, base);
        returnif(pop_syntax_only)
    endif;

    unless lablist /== [] or elselab then
        mishap(0, 'sysGO_ON: NO LABELS SUPPLIED')
    endunless;

    [% for lab in lablist do Get_goto_label(lab, true) endfor %]
                                        -> go_instr!INST_ARGS[_0];
    elselab and Get_goto_label(elselab, true) -> go_instr!INST_ARGS[_1];
    base -> go_instr!INST_ARGS[_2]
enddefine;


define lconstant Check_code_p(p, _need1);
    lvars p, _mltp = pdprops(p), _need1;
    unless isinteger(_mltp) and _mltp fi_>= 0 then
        mishap(p, 1, 'CODE-PLANTING PROCEDURE FOR DLOCAL EXPRESSION NEEDED')
    elseif _need1 then
        if _mltp /== 1 then
            mishap(p, 1, 'MULTIPLICITY OF DLOCAL EXPRESSION MUST BE 1')
        endif
    else
        _mltp
    endunless
enddefine;

protected
define vars sysLOCAL(token);
    lvars   id, token, pcr = vm_current_pcr, d_list, tmp,
            _idprops, _nonactive, _temp, _mltp;

    define lconstant expr_local(nvars, code_pdr, actid);
        lvars   nvars, actid, entry;
        dlvars  procedure code_pdr;

        define lconstant plant_code(upd);
            lvars   startpair = 0::[], upd;
            dlocal  vm_code_end = startpair, vm_code_penult = false,
                    _simstack = _S_NONE, _vm_stack_count = _0;
            if upd then -> code_pdr() else code_pdr() endif;
            ;;; return code produced
            fast_back(startpair)
        enddefine;

        ;;; entry in dlexpr list is a 3- or 4-element vector
        ;;;     { save-lvars access-code update-code actid }
        ;;; where actid is present only for an active variable

        initv(if actid then 4 else 3 endif) -> entry;
        [% repeat nvars times New_save_lvar(actid) endrepeat %]
                            -> entry!DLX_VARS;
        plant_code(false)   -> entry!DLX_GET_CODE;
        plant_code(true)    -> entry!DLX_PUT_CODE;
        if actid then actid -> entry!DLX_ACTID endif;

        ;;; assoc list entry for this dlexpr
        actid :: (entry :: vm_current_pcr!PCR_DLEXPR_LIST)
                                -> vm_current_pcr!PCR_DLEXPR_LIST
    enddefine;


    if isprocedure(token) then
        ;;; code-planting procedure for dlocal expression
        Check_code_p(token, false) -> _mltp;
        if popexecute then
            ;;; top-level -- make dlocal_context/process compile to
            ;;; push of vm_dlocal_context/process
            dlocal compiling_top_dlexpr = true;
            sysPROCEDURE(false, 0), token(), sysENDPROCEDURE() -> tmp;
            sysPROCEDURE(false, _mltp), -> token(), sysENDPROCEDURE()
                                                        -> updater(tmp);
            dlocal vm_dlocal_context = 1;
            tmp :: (consvector(fast_apply,fast_apply(tmp),_mltp fi_+ 1)
                    :: pcr!PCR_DLEXPR_LIST) -> pcr!PCR_DLEXPR_LIST
        else
            expr_local(_mltp, token, false)
        endif;
        return
    endif;

    sys_use_current_ident(token) -> (id, _nonactive);
    id!ID_IDENTPROPS -> _idprops;
    if _idprops _bitst _:M_ID_ACTIVE and not(_nonactive) then
        ;;; dlocal active var
        returnif(id == ident _);
        if _idprops _bitst _:M_ID_OPERATOR then
            1
        else
            _pint(id!ID_NUM_ATTRIBUTE)  ;;; multiplicity
        endif -> _mltp;
        pcr!PCR_DLEXPR_LIST -> d_list;
        if popexecute then
            ;;; top-level
            unless (list_assoc_val(id, d_list) ->> tmp)
            and fast_subscrv(1,tmp) == idval then
                id :: (consvector(idval,idval(id),_mltp fi_+ 1) :: d_list)
                            -> pcr!PCR_DLEXPR_LIST
            endunless
        else
            unless list_assoc(id, d_list) then
                expr_local(_mltp, Plant_call(%id%), id)
            endunless
        endif
    else
        ;;; ordinary identifier
        if Checkr_assign(_idprops) ->> _temp then
            mishap(token, 1, 'DECLARING ' <> _temp <> ' AS DYNAMIC LOCAL',
                                        inval_idstring)
        endif;
        if _idprops _bitst _:M_ID_LEX_TOKEN
        and not(id!ID_LEX_FLAGS _bitst _:M_LEX_NON_LOCAL) then
            mishap(token, 1, 'DECLARING LEXICAL LOCAL AS DYNAMIC LOCAL',
                                        inval_idstring)
        endif;
        if popexecute then
            ;;; top-level
            pcr!PCR_DLEXPR_LIST -> d_list;
            unless (list_assoc_val(id, d_list) ->> tmp)
            and fast_subscrv(1,tmp) == fast_idval then
                ;;; ordinary idents go last
                d_list nc_<> [% id, consvector(fast_idval,fast_idval(id),2) %]
                            -> pcr!PCR_DLEXPR_LIST;
                if vm_pas_mode == "popc" then
                    valof("popc_compstream_dlocal")(id)
                endif
            endunless
        else
            pcr!PCR_DYN_VARS -> d_list;
            unless fast_lmember(id, d_list) then
                id :: d_list -> pcr!PCR_DYN_VARS
            endunless
        endif
    endif
enddefine;


define lconstant Lookup_lex(token);
    lvars token, l = vm_lex_assoc, lblk_end, lblk = vm_current_lblk;
    if lblk!LBLK_LEVEL == 0 then
        []
    else
        lblk!LBLK_OUTER_BLOCK!LBLK_LEX_ASSOC
    endif -> lblk_end;
    until l == [] do
        if l == lblk_end then false -> lblk_end endif;
        returnif(fast_front(l) == token) (fast_front(fast_back(l)), lblk_end);
        fast_back(fast_back(l)) -> l
    enduntil;
    false, lblk_end
enddefine;


define lconstant Do_lex_check(word);
    lvars l, id, word, _flags;
    returnunless(Lookup_lex(word) -> ->> id);
    ;;; we only allow a lex identifier to be redeclared as dynamic
    ;;; if it's non-local and unused
    id!ID_LEX_FLAGS -> _flags;
    if _flags _bitst _:M_LEX_NON_LOCAL and not(_flags _bitst _:M_LEX_USED)
    or pop_vm_flags &&/=_0 VM_MIX_NONLOCAL_AND_LOCAL_LEX then
        ;;; remove the lexical scoping for the current procedure
        vm_lex_assoc -> l;
        until fast_front(l) == word do fast_back(fast_back(l)) -> l enduntil;
        false -> fast_front(l)
    else
        mishap(word, 1, 'ILLEGAL REDECLARATION OF LEXICAL IDENTIFIER',
                                ambig_idstring)
    endif
enddefine;

define lconstant Perm_declare(token, idprops, const);
    lvars token, idprops, const;
    returnif(pop_syntax_only);
    ident_declare(token, idprops, const);
    if vm_pas_mode then
        valof("pas_declare_perm")(token!W_IDENTIFIER)
    endif
enddefine;

protected
define vars sysSYNTAX(token, idprops, const);
    lvars token, idprops, const;
    Check_token(token);
    Do_lex_check(token);
    Perm_declare(token, idprops, const)
enddefine;

protected
define vars sysVARS(word, idprops);
    lvars id, word, idprops, _idprops;
    Check_word(word);
    Do_lex_check(word);
    if pop_vm_compiling_list == [] then
        Perm_declare(word, idprops, false)
    else
        ;;; Ghastly mess that's eliminated by declaring -vars- at top
        ;;; level only and then using -dlocal- inside procedures.
        if ispair(idprops) then
            mishap(word, 1, 'ACTIVE DECLARATION INSIDE PROCEDURE')
        endif;
        if iscompound(word!W_IDENTIFIER ->> id) then
            if (id!ID_IDENTPROPS ->> _idprops) _bitst _:M_ID_ACTIVE then
                if id <@(w) _system_end then chain(word, sysLOCAL) endif
            elseif _idprops _bitst _:M_ID_CONST_OR_PROT
            and pop_vm_flags &&=_0 VM_NOPROT_PVARS then
                mishap(word, 1, 'DECLARING CONSTANT OR PROTECTED VAR AS DYNAMIC LOCAL',
                                    inval_idstring)
            endif
        endif;
        Perm_declare(word, idprops, false);
        sysLOCAL(word)
    endif
enddefine;

protected
define vars sysCONSTANT(word, idprops);
    lvars word, idprops;
    Check_word(word);
    Do_lex_check(word);
    if pop_vm_compiling_list /== [] then
        mishap(word, 1, 'CONSTANT DECLARATION INSIDE PROCEDURE')
    endif;
    Perm_declare(word, idprops, true)
enddefine;


define lconstant Is_dlocal(id);
    lvars pcr = vm_current_pcr, id;
    fast_lmember(id, pcr!PCR_DYN_VARS) or list_assoc(id, pcr!PCR_DLEXPR_LIST)
enddefine;

define Is_local_var(token);
    lvars id, token, _in_curr_lblk;
    if (Lookup_lex(token) -> _in_curr_lblk ->> id)
    and not(id!ID_LEX_FLAGS _bitst _:M_LEX_NON_LOCAL) then
        _in_curr_lblk
    else
        (sys_current_ident(token) ->> id) and (popexecute or Is_dlocal(id))
    endif and id
enddefine;

define Cons_lex_id(_idprops, _num) -> id;
    lvars id, _idprops, _num;
    Get_record(ident_key) -> id;
    _idprops _biset _:M_ID_LEX -> id!ID_IDENTPROPS; ;;; make it lexical
    _0 -> id!ID_LEX_FLAGS;
    _num -> id!ID_NUM_ATTRIBUTE;
    0 -> fast_idval(id);
    unless _idprops _bitst _:M_ID_LEX_TOKEN then
        Make_idval_undef(id)
    endunless
enddefine;

define Create_lex(key_item, _idprops, _num, use_dynamic) -> id;
    lvars id, key_item, use_dynamic, _idprops, _num;
    if popexecute then
        ;;; real identifier at top-level
        Cons_lex_id(_idprops, _num) -> id
    else
        ;;; is just a local token, not (yet) a real identifier
        Cons_lex_id(_idprops _biset _:M_ID_LEX_TOKEN, _num) -> id;
        ;;; add to current lvar environment
        cons_assoc(id, id, vm_current_lblk!LBLK_LVAR_ENV)
                                            -> vm_current_lblk!LBLK_LVAR_ENV;
        if use_dynamic then _:M_LEX_USE_DYNAMIC -> id!ID_LEX_FLAGS endif
    endif;
    key_item :: (id :: vm_lex_assoc) -> vm_lex_assoc;

    if vm_pas_mode then
        ;;; attached name under which id declared
        key_item -> valof("pas_ident_token")(id)
    endif
enddefine;

define lconstant Declare_lex(word, idprops, const, use_dynamic);
    lvars   id, idprops, word, use_dynamic, const, _idprops, _num,
            _in_curr_lblk, _actmult = false;
    Check_token(word);
    unless idprops do   ;;; false means cancel lvar
        Do_lex_check(word); return
    endunless;
    if iscompound(word!W_IDENTIFIER)
    and word!W_IDENTIFIER!ID_IDENTPROPS _bitst _:M_ID_PROTECT
    and pop_vm_flags &&=_0 VM_NOPROT_LVARS
    then
        mishap(word, 1, 'idw: ILLEGAL DECLARATION OF WORD (missing ; after lvars?)',
                                'vm-ident-lex:name-decl-prot')
    endif;

    Idprops_flags(word, idprops, const) -> (_idprops, _num);

    if Lookup_lex(word) -> _in_curr_lblk ->> id then
        ;;; already lexically declared
        if id!ID_LEX_FLAGS _bitst _:M_LEX_NON_LOCAL then
            ;;; currently non-local
            if id!ID_LEX_FLAGS _bitst _:M_LEX_USED
            and pop_vm_flags &&=_0 VM_MIX_NONLOCAL_AND_LOCAL_LEX then
                ;;; already used non-locally, and not allowed to mix
                mishap(word, 1, 'REDECLARING NON-LOCAL LEXICAL IDENTIFIER AS LOCAL',
                                    ambig_idstring)
            endif
        elseif _in_curr_lblk then
            ;;; just return if declaration is the same
            returnif(id!ID_IDENTPROPS _biclear (_:M_ID_ASSIGNED_CONST
                        _biset _:M_ID_LEX_TOKEN _biset _:M_ID_LEX) == _idprops
                    and id!ID_NUM_ATTRIBUTE == _num);
            mishap(word, idprops, 2, 'REDECLARING LEXICAL IDENTIFIER STATUS',
                                            ambig_idstring)
        endif
    elseif iscompound(word!W_IDENTIFIER ->> id) and Is_dlocal(id)
    and pop_vm_flags &&=_0 VM_MIX_NONLOCAL_AND_LOCAL_LEX then
        mishap(word, 1, 'LEXICAL IDENTIFIER IS ALREADY A DYNAMIC LOCAL',
                                    ambig_idstring)
    endif;

    ;;; construct new id and add to vm_lex_assoc
    Create_lex(word, _idprops, _num, use_dynamic) -> id;
    id!ID_IDENTPROPS -> _idprops;
    if _idprops _bitst (_:M_ID_SYNTAX _biset _:M_ID_MACRO)
    and _idprops _bitst _:M_ID_LEX_TOKEN
    and not(_idprops _bitst _:M_ID_CONSTANT)
    then
        mishap(word, idprops, 2, 'ILLEGAL VALUE FOR LEXICAL VARIABLE IDENTPROPS');
    endif
enddefine;

protected
define vars sysNEW_LVAR() -> word;
    lvars word;
    hd(pop_new_lvar_list) -> word;
    unless pop_syntax_only then
        tl(pop_new_lvar_list) -> pop_new_lvar_list
    endunless;
    Declare_lex(word, 0, false, false)
enddefine;

protected
define vars sysLVARS     = Declare_lex(%false, false%) enddefine;
protected
define vars sysDLVARS    = Declare_lex(%false, true%) enddefine;
protected
define vars sysLCONSTANT = Declare_lex(%true, false%) enddefine;

define Passign_lconst(item, id, word);
    lvars item, id, word, _idprops, _in_curr_lblk;
    unless (Lookup_lex(word) -> _in_curr_lblk) == id and _in_curr_lblk then
        mishap(word, 1, 'NON-LOCAL ASSIGNMENT TO LCONSTANT')
    endunless;
    id!ID_IDENTPROPS -> _idprops;
    if _idprops _bitst _:M_ID_ASSIGNED_CONST then
        mishap(word, 1, 'ILLEGAL ASSIGNMENT TO LCONSTANT')
    elseif _idprops _bitst _:M_ID_PROCEDURE_VAL then
        Checkr_pdrpop(item, word) ->
    endif;
    Action_lex_pushes();
    if not(popexecute) and isprocedure(item) and not(item!PD_UPDATER) then
        Retrieve_pcr(item) -> item
    endif;
    if id!ID_LEX_FLAGS _bitst _:M_LEX_COUNTED_PUSH and Is_pcr(item) then
        ;;; set this pcr pushed
        item!PCR_FLAGS fi_|| M_PCR_PUSHED -> item!PCR_FLAGS
    endif;
    ;;; assign to the identifier
    _idprops _biset _:M_ID_ASSIGNED_CONST -> id!ID_IDENTPROPS;
    item -> fast_idval(id)
enddefine;

vars procedure sysUPASSIGN;

protected
define vars sysPASSIGN(item, token);
    lvars id, token, item, word, _nonactive;

    define lconstant Do_passign(item, id, word, _nonactive);
        lvars current, id, item, ucurrent, word, tracedpdr, _nonactive;
        returnif(pop_syntax_only);
        if isprocedure(item) then
            if _nonactive then fast_idval(id) else idval(id) endif -> current;
            if isprocedure(current) and not(isundef(current)) then
                current!PD_UPDATER -> ucurrent;
                if isclosure(current)
                and current!PD_CLOS_PDPART == weakref systrace
                and fast_frozval(2, current) == word then
                    ;;; currently traced: trace new procedure too
                    fast_frozval(1, current) -> tracedpdr;
                    if tracedpdr!PD_UPDATER and not(item!PD_UPDATER) then
                        tracedpdr!PD_UPDATER -> item!PD_UPDATER
                    endif;
                    ;;; *MUST* copy closure before changing its frozval
                    ;;; because users may have other pointers to it
                    copy(current) -> current;
                    item -> fast_frozval(1, current);
                    current -> item
                elseif ucurrent and not(item!PD_UPDATER) then
                    ucurrent -> item!PD_UPDATER
                endif
            endif
        endif;
        ;;; assign to the identifier
        if _nonactive then
            Nonactive_assign(item, id, word)
        else
            item -> idval(id)
        endif
    enddefine;

    if isprocedure(token) then
        ;;; code-planting procedure
        Check_code_p(token, true);
        sysPUSHQ(item) -> token();
        return
    endif;

    if (Passign_ident(token) -> (id, word, _nonactive)) then
        ;;; lconstant
        Passign_lconst(item, id, word)
    elseif id == ident _ and not(_nonactive) then
        ;;; anonymous var -- do nothing
        return
    elseif not(popexecute) or vm_pas_mode then
        ;;; plant code to do it
        SysPUSHQ(item), SysPOP(token)
    ;;; assigning item at top-level
    elseif Code_empty() and not(Is_pcr(item)) then
        ;;; do it right away
        Do_passign(item, id, word, _nonactive)
    else
        ;;; have to plant code to do it
        SysPUSHQ(item);
        SysCALLQ(Do_passign(%id, word, _nonactive%))
    endif
enddefine;
;;;
define updaterof sysPASSIGN(/*pdr, token*/) with_nargs 2;
    chain(/*pdr, token*/, sysUPASSIGN)
enddefine;

protected
define vars sysUPASSIGN(pdr, token);
    lvars id, token, pdr, base, word, _idprops, _nonactive, _in_curr_lblk;

    define lconstant Do_upassign(pdr, id, word, _nonactive);
        lvars current, id, pdr, word, _nonactive;
        returnif(pop_syntax_only);
        if _nonactive then fast_idval(id) else idval(id) endif -> current;
        if isclosure(current)
        and current!PD_CLOS_PDPART == weakref systrace
        and fast_frozval(2, current) == word then
            ;;; just give basic procedure new updater
            pdr -> fast_frozval(1,current)!PD_UPDATER;
            return
        elseunless isprocedure(current) and not(isundef(current)) then
            consclosure(Exec_only_updater, 0, 1) ->> current
                -> if _nonactive then fast_idval(id) else idval(id) endif;
            current -> fast_frozval(1, current);
            pdr!PD_PROPS -> pdprops(current)
        endif;
        pdr -> updater(current)
    enddefine;

    Check_procedure(pdr);

    if isprocedure(token) then
        ;;; code-planting procedure
        Check_code_p(token, true);
        -> sysCALL(sysPUSHQ(pdr), token(), "updater");
        return
    endif;

    if (Passign_ident(token) -> (id, word, _nonactive)) then
        ;;; lconstant
        unless (Lookup_lex(word) -> _in_curr_lblk) == id and _in_curr_lblk then
            mishap(word, 1, 'NON-LOCAL ASSIGNMENT TO LCONSTANT UPDATER')
        endunless;
        id!ID_IDENTPROPS -> _idprops;
        fast_idval(id) -> base;
        unless _idprops _bitst _:M_ID_ASSIGNED_CONST and isprocedure(base) then
            mishap(word, 1, 'NO BASE PROCEDURE FOR LCONSTANT UPDATER')
        elseif base!PD_UPDATER then
            mishap(word, 1, 'REASSIGNING LCONSTANT UPDATER')
        endunless;
        Action_lex_pushes();
        if Is_pcr(pdr) or Is_pcr(base) then
            Retrieve_pcr(base) ->> base -> fast_idval(id);
            Retrieve_pcr(pdr) -> pdr;
            if base!PCR_PDR_REC and pdr!PCR_PDR_REC then
                pdr!PCR_PDR_REC -> base!PCR_PDR_REC!PD_UPDATER
            endif;
            ;;; if base procedure pushed, set updater pushed also
            pdr!PCR_FLAGS fi_|| (base!PCR_FLAGS fi_&& M_PCR_PUSHED)
                                -> pdr!PCR_FLAGS
        endif;
        pdr -> base!PD_UPDATER;
        return
    endif;
    unless popexecute then
        ;;; local
        SysUCALL(SysPUSHQ(pdr), SysPUSH(token), "updater")
    elseif vm_pas_mode then
        unless pop_syntax_only then
            SysPUSHQ(pdr);
            Plant("pas_UPASSIGN", id, 2)
        endunless
    elseif Code_empty() and not(Is_pcr(pdr)) then
        ;;; do it right away
        Do_upassign(pdr, id, word, _nonactive)
    else
        ;;; have to plant code to do it
        SysPUSHQ(pdr);
        SysCALLQ(Do_upassign(%id, word, _nonactive%))
    endunless
enddefine;

protected
define vars sysCONSTRUCT(compile_p, type);
    lvars   l, codelist, reprocess, count, callname, type, len_var,
            compile_p, save_call, my_lvar_list,
            save_penult = vm_code_penult,
            save_endcode = vm_code_end,
            save_reprocess = vm_reprocess_instrs;
    dlocal  pop_new_lvar_list;

    unless pop_syntax_only then
        if (type == SCON_VECONS or type == SCON_VECTOR)
        and not(pop_syntax_only) then
            ;;; reserve a temp lvar without declaring it
            dest(pop_new_lvar_list) -> (len_var, pop_new_lvar_list)
        endif;
        ;;; plant a dummy instruction so sysEXEC_COMPILE won't call sysEXECUTE
        Plant(identfn, 1)
    endunless;

    compile_p() -> (count, callname);
    returnif(pop_syntax_only);

    fast_back(save_endcode) -> codelist;    ;;; starts with dummy instr

    if count then
        ;;; fixed number of elements
        vm_code_end -> save_call;
        sysCALL(callname);
        [] -> fast_back(save_endcode);
        (save_penult, save_endcode) -> (vm_code_penult, vm_code_end);

        if vm_reprocess_instrs /== save_reprocess then
            vm_reprocess_instrs ->> reprocess -> l;
            until fast_back(l) == save_reprocess do fast_back(l) -> l enduntil;
            [] -> fast_back(l);
            save_reprocess -> vm_reprocess_instrs
        else
            [] -> reprocess
        endif;

        Plant_reprocess_next();
        Plant(rI_CONSTRUCT, fast_back(codelist), fast_back(save_call),
                                reprocess, type, count, 6)

    else
        [] -> fast_back(save_endcode);
        vm_code_penult, vm_code_end, save_penult, save_endcode
                -> (save_penult, save_endcode, vm_code_penult, vm_code_end);

        if type == SCON_LIST then
            SysPUSH("popstackmark")
        elseunless type == SCON_RECORD then
            Declare_lex(len_var, 0, false, false);
            SysCALL("stacklength") -> SysPUSH(len_var)
        endif;

        fast_back(codelist) -> codelist;    ;;; remove dummy instr
        if codelist /== [] then
            codelist -> fast_back(vm_code_end);
            if fast_back(codelist) /== [] then
                save_penult
            else
                vm_code_end
            endif -> vm_code_penult;
            save_endcode -> vm_code_end
        endif;

        if type == SCON_VECONS then
            SysPUSH(len_var)
        elseif type == SCON_VECTOR then
            SysCALL(SysCALL("stacklength"), SysPUSH(len_var), "fi_-")
        endif;

        sysCALL(callname)
    endif
enddefine;


endsection;     /* $-Sys$-Vm */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 17 1996
        Changed format of I_GO_ON to include base integer value
--- John Gibson, Sep 27 1996
        Changes to keep new label reference counts updated.
--- John Gibson, Sep 16 1996
        Changed appropriate calls of sys_use_current_ident to pass true
        for the new 'try no-fast' 2nd arg.
--- John Gibson, Apr 26 1996
        Changed sysGO_ON to allow optional 3rd arg to specify base integer,
        or to take procedure that returns the sysGO_ON args.
--- John Gibson, Apr  9 1996
        Added some mishap idstrings
--- John Gibson, Mar 26 1996
        Changes to sysLOCAL to allow it to be used at execute level.
--- John Gibson, Mar  5 1996
        Made sysPUSH, CALL etc treat assigned non-system perm constants as
        vars if pop_debugging == true.
--- John Gibson, Jan 15 1996
        Added appropriate tests to optimise new anonymous var _ .
--- John Gibson, Dec 21 1995
        Stopped popexecute being true from preventing sysCONSTRUCT using
        rI_CONSTRUCT
--- John Gibson, Jun 24 1995
        Added sysCONSTRUCT
--- John Gibson, Jun  3 1995
        Changed sysUCALLQ to use Is_updateable to decide whether it can
        call a procedure's updater separately from the base
--- John Gibson, May 27 1995
        Changed sysLOCAL to add the active identifier to dlocal active
        var expression entries.
--- John Gibson, May 22 1995
        Made Is_local_var return the identifier instead of true
--- John Gibson, Oct 18 1994
        free*pairs -> _free_pairs
--- John Gibson, Aug 11 1994
        Changed Declare_lex so that mishap for redeclaring a dlocal identifier
        as lexical does not happen if VM_MIX_NONLOCAL_AND_LOCAL_LEX is set.
--- John Gibson, Aug 16 1993
        Replaced pop_p*as_mode with internal var vm_pas_mode
--- John Gibson, May 21 1993
        Moved sys_current_ident, sys_use_current_ident and sys_current_val
        to new file sys_current_ident.p
--- John Gibson, May  4 1993
        Moved Moveq_op to vm_conspdr.p
--- John Gibson, May  2 1993
        Adjustments to behaviour of sys_use_current_ident when
        pop_syntax_only is true
--- John Gibson, Nov  3 1992
        Added sys_current_val
--- John Gibson, Oct 16 1992
        Changed sys_use_current_ident to deal with a vector of dependency
        identifiers, and other changes for POPC
--- John Gibson, Oct  3 1992
        Fix to sys_current_ident for POPC
--- John Gibson, Nov  9 1990
        Made -sys_use_current_ident- just return a dummy lex identifier when
        word undeclared and -pop_syntax_only- true.
--- John Gibson, Jun 27 1990
        Changed updaterof sysPUSH etc to chain the corresponding update
        procedures rather than call them.
--- John Gibson, Aug  4 1989
        -sys_current_ident- and -sys_use_current_ident- now return a dummy
        lexical identifier for any word in -pop_vm_dummy_idents-.
--- John Gibson, Jun  9 1989
        -pop_debugging- true makes various VM_ flags ineffective.
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, May  9 1989
        pop_vm_compiling_list /== [] is now the appropriate test for being
        inside procedure compilation (since popexecute can now be false
        inside a non-executing lblock).
--- John Gibson, May  2 1989
        Unprotecting perm identifiers now done with -pop_vm_flags-.
        Other changes for popc to compile checks where appropriate.
--- John Gibson, Apr 30 1989
        Put into section $-Sys$-Vm.
--- John Gibson, Apr 27 1989
        I_CHECK instructions for backward jumps now planted after the
        target label rather than before the jump instruction
--- John Gibson, Apr 16 1989
        Changed sysPASSIGN to allow assignment of any item to an identifier
        (i.e. not just a procedure).
--- John Gibson, Feb 17 1989
        Allowed sys(U)PASSIGN to take a "token" arg which is actually a
        code-planting procedure (as for sysLOCAL).
--- John Gibson, Jan 29 1989
        Changes for new popc
--- John Gibson, Dec 18 1988
        -Vm_use_current_ident- renamed and exported as -sys_use_current_ident-
--- John Gibson, Nov 23 1988
        Changes for lexical blocks.
--- John Gibson, Apr 14 1988
        Fixed bug in -sys_use_current_ident- (happened when -vm_pas_mode-
        was true).
--- John Gibson, Mar 27 1988
        -list_assoc- etc into section Sys
--- John Gibson, Mar 11 1988
        Name changed to vm_plant.p (previously vmplant.p)
--- John Gibson, Feb 29 1988
        Moved sys(U)FIELD_VAL to vm_fields.p
--- John Gibson, Feb 28 1988
        Weakref'ed -systrace-
--- John Gibson, Feb 11 1988
        Check_integer in section Sys
--- John Gibson, Jan 19 1988
        Altered -sys_current_ident- to assign to -pas_ident_token- for
        a perm identifier when -vm_pas_mode- true
 */
