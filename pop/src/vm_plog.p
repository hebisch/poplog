/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/src/vm_plog.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *PROLOG
 */

;;;------------ PROLOG VIRTUAL MACHINE INSTRUCTIONS -------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'vmdefs.ph'

global constant
        procedure (consprologterm, Sys$-Plog$-Assign_term,
        sys_use_current_ident, isprologterm, Sys$-cons_assoc
        ),
        _prolog_assign, _prolog_assign_pair, _conspair,
        _prolog_pair_switch, _prolog_term_switch,
        _prolog_unify_atom, _prolog_restore
    ;

section $-Sys$-Vm;

global constant
        procedure (I_CALLSUB_REG, I_PLOG_TERM_SWITCH, I_PLOG_IFNOT_ATOM,
        I_PLOG_RESTORE, Get_goto_label, Plant_label, SysPUSHQ, SysPUSH,
        SysCALL, Plant_pop, Moveq_op)
    ;

global vars
        asm_clist, asm_instr, _asm_pdr_flags
    ;

endsection;


;;; ----------------------------------------------------------------------

section $-Sys$-Vm => sysPLOG_SAVE, sysPLOG_RESTORE, sysPLOG_RESTART,
                     sysPLOG_ARG_PUSH, sysPLOG_IFNOT_ATOM, sysPLOG_TERM_SWITCH;


;;; --- OPTIMISING PROLOG PROCEDURE CALLS --------------------------------

    ;;; optimise -prolog_assign-
define Opt_plog_assign();
    lvars instr, op, pdr;
    -> ;                ;;; erase dummy arg
    _S_NONE -> _simstack;
    if isvector(fast_front(vm_code_end) ->> instr) then
        instr!INST_OP -> op;
        if op == I_CALLABS then
            if (instr!INST_ARGS[_0] ->> pdr) == consprologterm then
                $-Sys$-Plog$-Assign_term -> instr!INST_ARGS[_0];
                return(true)
            elseif pdr == conspair then
                I_CALLSUB -> instr!INST_OP;
                _prolog_assign_pair -> instr!INST_ARGS[_0];
                return(true)
            endif
        elseif op == I_CALLSUB and instr!INST_ARGS[_0] == _conspair
        then
            _prolog_assign_pair -> instr!INST_ARGS[_0];
            return(true)
        endif
    endif;
    Plant(I_CALLSUB, _prolog_assign, 2);
    true
enddefine;


;;; --- OPTIMISATIONS FROM WITHIN Consprocedure ----------------------------

    ;;; {I_PLOG_RESTART <dummy>}
define lconstant I_PLOG_RESTART();
    if _asm_pdr_flags _bitst _:M_PD_PLOG_CHOICE then
        ;;; call _prolog_save_check to resave the prolog globals
        _prolog_save_check
    else
        ;;; if not actaully using PLOG_SAVE/RESTORE, just check trail etc
        _checkplogall
    endif -> asm_instr!INST_ARGS[_0];
    fast_chain(I_CALLSUB ->> asm_instr!INST_OP)
enddefine;


;;; --- VM INSTRUCTIONS ---------------------------------------------------


    ;;; Currently only used for possible reg caching of
    ;;; prolog subroutinesa
define lconstant Pcr_lit_cache(lit, _ntimes_to_cache) -> countpair;
    lvars pcr = vm_current_pcr, countpair, lit, _ntimes_to_cache;
    if list_assoc_val(lit, pcr!PCR_LITERAL_CACHE) ->> countpair then
        fast_front(countpair) fi_+ 1 -> fast_front(countpair)
    else
        conspair(1, _ntimes_to_cache) -> countpair;
        cons_assoc(lit, countpair, pcr!PCR_LITERAL_CACHE)
                                                -> pcr!PCR_LITERAL_CACHE
    endif
enddefine;

protected
define vars sysPLOG_SAVE();
    lvars pcr = vm_current_pcr;
    returnif(pop_syntax_only);
    if ispair(pcr!PCR_PLOG_PDR) then
        ;;; this SAVE follows another one
        mishap(0, 'sysPLOG_SAVE: sysPLOG_RESTORE EXPECTED')
    endif;
    ;;; SAVE doesn't generate any code, but remember its position
    ;;; in case immediately followed by an IFNOT_ATOM
    unless fast_front(vm_code_end) then
        ;;; fill the unplanted slot with a no-op
        Plant(if vm_pas_mode then "pas_NOOP" else identfn endif, 1)
    endunless;
    ;;; remember position -- also marks as prolog procedure
    vm_code_end -> pcr!PCR_PLOG_PDR;
    _S_NONE -> _simstack
enddefine;

protected
define vars sysPLOG_RESTORE();
    lvars fail_inst, save, lab, pcr = vm_current_pcr;
    returnif(pop_syntax_only);
    unless ispair(pcr!PCR_PLOG_PDR) then
        ;;; RESTORE with no preceding SAVE
        mishap(0, 'sysPLOG_RESTORE: NO PRECEDING sysPLOG_SAVE')
    endunless;

    ;;; mark as prolog procedure
    true -> pcr!PCR_PLOG_PDR;
    _S_NONE -> _simstack;

    ;;; see if there is an IFNOT_ATOM or TERM_SWITCH that immediately followed
    ;;; a SAVE and whose fail label jumps to this RESTORE
    false -> lab;
    if (pcr!PCR_PLOG_FAIL_INSTR ->> fail_inst)
    and fail_inst!INST_ARGS[_0] == vm_code_end then
        ;;; yes -- fail label is label for this RESTORE
        ;;; make it jump to after this RESTORE
        Get_goto_label(sysNEW_LABEL(), false) ->> lab
                                            -> fail_inst!INST_ARGS[_0]
    endif;
    false -> pcr!PCR_PLOG_FAIL_INSTR;

    if vm_pas_mode then
        Plant("pas_PLOG_RESTORE", 1)
    else
        ;;; use a pair to count the number of uses of _prolog_restore.
        ;;; I_CALLSUB_REG later extracts either the s/r name or a reg allocated
        ;;; to it.
        Plant(I_CALLSUB_REG, Pcr_lit_cache(_prolog_restore, 8), 2)
    endif;

    if lab then Plant_label(lab, false) endif
enddefine;

protected
define vars sysPLOG_RESTART(label);
    lvars label;
    returnif(pop_syntax_only);
    Get_goto_label(label, false) -> label;  ;;; no back jump check required
    if vm_pas_mode then
        Plant("pas_PLOG_RESTART", 1);
        Plant("pas_GOTO", label, 2)
    else
        Plant(I_PLOG_RESTART, 0, 2);        ;;; 0 is dummy
        Plant(I_GOTO, label, I_BR, 3)
    endif;
    _S_NONE -> _simstack
enddefine;

protected
define vars sysPLOG_ARG_PUSH(item, qual);
    lvars item, qual;
    if isinteger(qual) then
        SysPUSHQ(qual);
        SysPUSH(item);
        SysCALL("fast_prolog_arg")
    elseif qual then
        SysPUSH(item);
        if qual /== true then SysCALL(qual) endif
    else
        SysPUSHQ(item)
    endif
enddefine;
;;;
constant procedure SysPLOG_ARG_PUSH = sysPLOG_ARG_PUSH;

protected
define vars sysPLOG_IFNOT_ATOM(token, tqual, atom, aqual, label);
    lvars id, token, tqual, atom, aqual, label, pcr = vm_current_pcr,
        _remember;

    ;;; remember if a SAVE immediately preceded this
    pcr!PCR_PLOG_PDR == vm_code_end -> _remember;

    ;;; unless arg is a simple identifier push, plant code for it
    if tqual /== true then
        SysPLOG_ARG_PUSH(token, tqual);
        false -> id
    else
        sys_use_current_ident(token) -> -> id;
        if id!ID_IDENTPROPS _bitst (_:M_ID_CONSTANT _biset _:M_ID_ACTIVE) then
            SysPUSH(token);
            false -> id
        endif
    endif;

    ;;; unless atom is a simple pushq, plant code for it
    if aqual then SysPLOG_ARG_PUSH(atom, aqual) endif;

    returnif(pop_syntax_only);
    _S_NONE -> _simstack;

    ;;; change the check subroutine to one that also
    ;;; checks for plog trail overflow
    pcr!PCR_FLAGS fi_|| M_PCR_PLOG_TRAIL_CHECK -> pcr!PCR_FLAGS;
    Get_goto_label(label, false) -> label;

    if vm_pas_mode then
        Plant("pas_PLOG_IFNOT_ATOM", label,
                    if aqual then false else consref(atom) endif,
                    if id then id else false endif,
                    4)
    else
        ;;; if atom pushed, bring it into arg reg 1
        if aqual then Plant_pop(ident arg_reg_1) endif;
        ;;; bring arg into arg reg 0
        if id then
            Plant(I_MOVE, id, ident arg_reg_0, 3)
        else
            Plant_pop(ident arg_reg_0)
        endif;
        unless aqual then
            Plant(Moveq_op(atom), atom, ident arg_reg_1, 3)
        endunless;

        ;;; use a pair to count the number of uses of _prolog_unify_atom.
        ;;; I_CALLSUB_REG later extracts either the s/r name or a reg allocated
        ;;; to it.
        Plant(I_CALLSUB_REG, Pcr_lit_cache(_prolog_unify_atom, 8), 2);
        Plant(I_PLOG_IFNOT_ATOM, label, I_BRCOND, 3)
    endif;

    if _remember then
        ;;; remember the IFNOT_ATOM instruction in PCR_PLOG_FAIL_INSTR
        fast_front(vm_code_end) -> pcr!PCR_PLOG_FAIL_INSTR
    endif
enddefine;

protected
define vars sysPLOG_TERM_SWITCH(/* token, qual, */ term, var_label,
                                                    fail_label) with_nargs 5;
    lvars term, var_label, fail_label, func, pcr = vm_current_pcr,
        fail_instr, _routine, _remember;

    ;;; remember if a PLOG_SAVE immediately preceded this
    pcr!PCR_PLOG_PDR == vm_code_end -> _remember;

    SysPLOG_ARG_PUSH(/* token, qual */);

    returnif(pop_syntax_only);
    Get_goto_label(var_label, false) -> var_label;
    Get_goto_label(fail_label, false) -> fail_label;

    unless ispair(term) or isprologterm(term) then
        mishap(term, 1, 'sysPLOG_TERM_SWITCH: INVALID TERM')
    endunless;

    if vm_pas_mode then
        Plant("pas_PLOG_TERM_SWITCH", fail_label, var_label, term, 4);
        fast_front(vm_code_end) -> fail_instr
    else
        ;;; bring arg into arg reg 0
        Plant_pop(ident arg_reg_0);
        if ispair(term) then
            _prolog_pair_switch -> _routine
        else
            ;;; put functor and length args into arg regs 1 & 2
            term!PGT_FUNCTOR -> func;
            Plant(Moveq_op(func), func, ident arg_reg_1, 3);
            Plant(I_MOVENUM, _pint(term!PGT_LENGTH), ident arg_reg_2, 3);
            _prolog_term_switch -> _routine
        endif;
        Plant(I_CALLSUB, _routine, 2);
        ;;; (fail_label must be same arg num as in I_PLOG_IFNOT instructions)
        Plant(I_PLOG_TERM_SWITCH, fail_label, I_BRCOND, var_label, I_BRCOND, 5);
        fast_front(vm_code_end) -> fail_instr;
        ;;; falls thru if arg is a pair/term
        Plant(I_MOVE, ident arg_reg_0, 2)
    endif;

    if _remember then
        ;;; remember the TERM_SWITCH instruction in PCR_PLOG_FAIL_INSTR
        fail_instr -> pcr!PCR_PLOG_FAIL_INSTR
    endif;
    _PUSH_simstack _biset _S_ANY _biset _S_XPUSH -> _simstack
enddefine;

endsection;     /* $-Sys$-Vm */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 16 1993
        Replaced pop_p*as_mode with internal var vm_pas_mode
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, Apr 30 1989
        Put into section $-Sys$-Vm.
--- John Gibson, Apr 27 1989
        sysPLOG_RESTART changed to take label as argument (so following
        sysGOTO unnecessary).
--- John Gibson, Mar 28 1989
        Changes to allow POPC to handle these instructions
--- John Gibson, Dec 18 1988
        -Vm_use_current_ident- renamed and exported as -sys_use_current_ident-
--- John Gibson, Jul 26 1988
        Changed way in which prolog subroutines are cached in registers.
--- John Gibson, Mar 11 1988
        Name changed to vm_plog.p (previously vmplog.p)
--- John Gibson, Feb 21 1988
        Transferred procedures -Opt_plog_assign- and -I_PLOG_RESTART-
        from vmoptim.p to this file.
 */
