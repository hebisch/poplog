/* --- Copyright University of Sussex 2003. All rights reserved. ----------
 > File:            C.all/src/vm_conspdr.p
 > Purpose:
 > Author:          John Gibson, Mar 11 1988 (see revisions)
 */

;;; ------------- CONSTRUCTING RUN-TIME PROCEDURES --------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'vmdefs.ph'
#_INCLUDE 'gctypes.ph'

constant
        procedure (consexternal_ptr, Sys$-Flush_procedure,
        Sys$-Cons_rt_idents_untyped, Sys$-Cons_rt_idents),
        Sys$-Gc$-pd_table_noscan_region,
    ;

vars
        _plog_save_contn_sp, Sys$- _inhibit_gc, Sys$- _free_npfront_pairs
    ;

section $-Sys$-Vm;

constant
        procedure (Do_consprocedure, Is_address_reg,
        I_CREATE_SF, I_LABEL, I_MOVEADDR,
        I_BOOL_opt, I_NBOOL_opt, I_GOTO_opt, I_IF_opt, I_IF_CMP,
        I_UNWIND_SF, I_CHAIN_REG, I_MOVE_CALLER_RETURN
        ),
        asm_pop_registers, asm_nonpop_registers
    ;

weak constant
        procedure (I_IF_TAG, I_FAST_+-_2, I_FAST_+-_3, I_FAST_+-_any,
        I_SWITCH_base, I_SWITCH)
    ;

endsection;


;;; ---------------------------------------------------------------------

section $-Sys$-Vm;

vars
    asm_instr,
    asm_clist,
    asm_struct_list,
    asm_exit_lab,
    asm_cllr_return_id,
    asm_chainp_lab,
    _asm_code_offset,
    _asm_drop_ptr,
    _asm_pass,
    _asm_offset_diff,
    _asm_pdr_flags,
    _asm_nplit_size,

    _Nlocals,
    _Nreg,
    _Npopreg,
    _Nstkvars,
    _Npopstkvars,
    _Nframewords,
;

lvars
    asm_nplit_list,
;


;;; --- I-CODE UTILITIES -----------------------------------------------

section $-Sys;

    ;;; Chains of free instruction vectors upto length 7
constant
    _freevectab = _INIT_NONPOP_STRUCT(word[8]);

define New_lextoken() -> id;
    lvars id;
    Get_record(ident_key) -> id;
    _:M_ID_LEX _biset _:M_ID_LEX_TOKEN -> id!ID_IDENTPROPS;
    _0 ->> id!ID_LEX_FLAGS -> id!ID_NUM_ATTRIBUTE;
    0 -> fast_idval(id)
enddefine;

endsection;     /* $-Sys */


define Cons_inst(_len) -> instvec;
    lvars instvec, _addr, _len, _offs;
    _int(_len) -> _len;
    if _len _lt _8 and iscompound(_freevectab!(w)[_len] ->> instvec) then
        instvec!INST_OP -> _freevectab!(w)[_len];
        if _len == _2 then
            -> (instvec!INST_OP, instvec!INST_ARGS[_0]);
            return
        else
            @@V_WORDS[_len] -> _offs
        endif
    else
        @@V_WORDS[_len] -> _offs;
        Get_store(_offs _sub @@POPBASE) -> instvec;
        vector_key -> instvec!KEY;
        _len -> instvec!V_LENGTH
    endif;
    instvec@(w){_offs} -> _addr;
    instvec@V_WORDS[_0] -> _len;
    while _addr >@(w) _len do
        -> _addr--!(w) -> _addr
    endwhile;
enddefine;

define Garbage_inst(instvec);
    lvars instvec, _ptr, _len;
    if isvector(instvec)
    and _nonzero(instvec!V_LENGTH ->> _len) and _len _lt _8
    ;;; INST_OP will be a vector (or 0) if this vector
    ;;; is already in the free list
    and isprocedure(instvec!INST_OP) then
        _freevectab@(w)[_len] -> _ptr;
        _ptr!(w) -> instvec!INST_OP;
        instvec -> _ptr!(w)
    endif
enddefine;

define lconstant Garbage_codelist(list);
    lvars list, lstpair = false, org = list;
    until list == [] do
        list -> lstpair;
        Garbage_inst(fast_destpair(list) -> list)
    enduntil;
    if lstpair then
        _free_pairs -> fast_back(lstpair);
        org -> _free_pairs
    endif
enddefine;

define Moveq_op(item);
    lvars item;
    if vm_pas_mode then
        "pas_PUSHQ"
    elseif issimple(item) then
        I_MOVENUM
    elseif item <@(w) _system_end then
        I_MOVEADDR
    else
        I_MOVEQ
    endif
enddefine;

define Cons_push(id);
    lvars id;
    Cons_inst(if vm_pas_mode then "pas_PUSH" else I_MOVE endif, id, 2)
enddefine;

define Cons_pushq(item);
    lvars item;
    Cons_inst(Moveq_op(item), item, 2)
enddefine;

define Cons_pop(id);
    lvars id;
    Cons_inst(if vm_pas_mode then "pas_POP" else I_POP endif, id, 2)
enddefine;

define Cons_callabs(pdr);
    lvars pdr;
    Cons_inst(I_CALLABS, pdr, 2)
enddefine;

define Incr_lab_refcount(lab);
    lvars lab, _flags = _int(fast_front(lab));
    unless _flags _bitst _:M_LAB_REFMAX then
        _pint(_flags _add _:M_LAB_REF1) -> fast_front(lab)
    endunless
enddefine;

define Cons_goto(lab);
    lvars lab;
    Cons_inst(  if vm_pas_mode then
                    "pas_GOTO", lab, 2
                else
                    I_GOTO, lab, I_BR, 3
                endif);
    Incr_lab_refcount(lab)
enddefine;

define Cons_if(lab, ifso);
    lvars lab, ifso, opnd_instr = false;
    if isvector(ifso) then (), lab, ifso -> (lab, ifso, opnd_instr) endif;
    Cons_inst(  if vm_pas_mode then
                    "pas_IF", lab, ifso, 3
                else
                    I_IF, lab, ifso, I_BRCOND, opnd_instr, 5
                endif);
    Incr_lab_refcount(lab)
enddefine;

define Cons_bool(lab, ifso);
    lvars lab, ifso, opnd_instr = false;
    if isvector(ifso) then (), lab, ifso -> (lab, ifso, opnd_instr) endif;
    Cons_inst(  if vm_pas_mode then
                    "pas_BOOL", lab, ifso, 3
                else
                    I_BOOL, lab, ifso, I_BRCOND, opnd_instr, 5
                endif);
    Incr_lab_refcount(lab)
enddefine;

define Newlab(clist);
    lvars clist;
    #_< M_LAB_INITIAL || M_LAB_PLANTED >_# :: clist
enddefine;

define Newlab_ref1(clist);
    lvars clist;
    #_< M_LAB_INITIAL || M_LAB_REF1 || M_LAB_PLANTED >_# :: clist
enddefine;

define Deref_lab(lab) -> lab;
    lvars lab;
    while ispair(fast_front(lab)) do
        fast_front(lab) -> lab
    endwhile
enddefine;


;;; --- OPTIMISED INSTRUCTIONS INSIDE Cons_procedure ------------------------


    /*  Replace the current instruction with code
    */
define lconstant Replace_instr(code);
    lvars code, clist = asm_clist, next;
    if isvector(code) then
        code
    else
        fast_back(clist) -> next;
        if code == [] then
            if next /== [] and isvector(fast_front(next)) then
                fast_front(next), fast_back(next)
            else
                identfn -> asm_instr!INST_OP;
                return
            endif
        elseunless isvector(fast_front(code)) then
            ;;; begins with label
            identfn -> asm_instr!INST_OP;
            asm_instr, code nc_<> next
        else
            fast_front(code), fast_back(code) nc_<> next
        endif -> fast_back(clist)
    endif ->> asm_instr -> fast_front(clist);
    fast_chain(asm_instr!INST_OP)
enddefine;


define :inline lconstant IS_UNOPT_JUMP(op);
    (if op == I_GOTO then I_GOTO_opt
    elseif op == I_IF then I_IF_opt
    elseif op == I_BOOL then I_BOOL_opt
    elseif op == I_NBOOL then I_NBOOL_opt
    else false
    endif)
enddefine;

define :inline lconstant IS_OPT_CONDITION(op, instr);
    ((op == I_IF_opt or op == I_BOOL_opt or op == I_NBOOL_opt)
        and not(instr!INST_ARGS[_3]))   ;;; no explicit operand
enddefine;


    /*  Optimise conditions (ie IFs, BOOLs and NBOOLs) and GOTOs
    */
define lconstant Optimise_conditions(_s_type, clist);
    lvars   list, clist, target, source, _s_type, _t_type, _sametest,
            ;;; new version, based on analysis by David Young and Steve Leach
            _test, _sense = true, _no_nots = true;

    ;;; insert a label after an instruction
    define lconstant Label_after(this) -> next;
        lvars next = fast_back(this), this;
        if isvector(fast_front(next)) then
            ;;; new one needed
            Newlab(next) ->> next -> fast_back(this)
        ;;; else it's already a label
        endif
    enddefine;

    fast_front(clist) -> source;
    ;;; change to optimised type
    _s_type -> source!INST_OP;
    ;;; deref the target label
    Deref_lab(source!INST_ARGS[_0]) ->> list -> source!INST_ARGS[_0];

    ;;; find target instruction
    repeat
        fast_back(list) -> list;        ;;; skip the label
        if isvector(fast_front(list) ->> target) then
            if target!INST_OP == I_NOT and _s_type /== I_GOTO_opt then
                not(_sense) -> _sense;
                ;;; new version, based on analysis by David Young and Steve Leach
                false -> _no_nots       ;;; note that I_NOT found
            else
                quitloop
            endif
        endif
    endrepeat;

    ;;; if target is unoptimised condition then optimise forward
    target!INST_OP -> _t_type;          ;;; type of target
    if IS_UNOPT_JUMP(_t_type) ->> _t_type then
        Optimise_conditions(_t_type, list)
    endif;
    target!INST_OP -> _t_type;          ;;; type of target now

    ;;; new version, based on analysis by David Young and Steve Leach
    if _t_type == I_GOTO_opt and _no_nots then
        ;;; just go to where it (now) goes
        target!INST_ARGS[_0] -> clist;
        goto CHANGE_LAB
    elseunless (_s_type == I_BOOL_opt or _s_type == I_NBOOL_opt)
    and IS_OPT_CONDITION(_t_type, target) then
        return
    endif;

    ;;; (N)BOOL jumping to an IF or (N)BOOL that hasn't got an explicit
    ;;; operand (i.e. that isn't effectively preceded by a push)
    target!INST_ARGS[_0] -> clist;          ;;; where target (now) goes
    source!INST_ARGS[_1] -> _test;          ;;; source test
    _test == target!INST_ARGS[_1] -> _sametest; ;;; if same test
    if _t_type == I_IF_opt then
        unless _sametest == _sense then
            ;;; go to after it
            Label_after(list) -> clist
        endunless;
        if _s_type == I_NBOOL_opt then not(_test) -> _test endif;
        I_IF_opt -> _s_type             ;;; becomes I_IF
    elseif (_t_type == I_BOOL_opt) == _sense then
        unless _sametest then
            Label_after(list) -> clist; ;;; go to after target
            if _s_type == I_NBOOL_opt then not(_test) -> _test endif;
            I_IF_opt -> _s_type         ;;; becomes I_IF
        endunless
    else
        ;;; target is I_NBOOL_opt
        if _sametest then
            Label_after(list) -> clist; ;;; go to after target
            if _s_type == I_NBOOL_opt then not(_test) -> _test endif;
            I_IF_opt -> _s_type         ;;; becomes I_IF
        else
            not(_test) -> _test;
            if _s_type == I_BOOL_opt then I_NBOOL_opt else I_BOOL_opt endif
                -> _s_type
        endif
    endif;

    _s_type -> source!INST_OP;          ;;; insert source type
    _test -> source!INST_ARGS[_1];      ;;; insert new test

CHANGE_LAB:
    ;;; decrement ref count for old label
    source!INST_ARGS[_0] -> list;       ;;; old label
    _int(fast_front(list)) -> _test;
    if _neg(_test) and not(_test _bitst _:M_LAB_REFMAX) then
        _pint(_test _sub _:M_LAB_REF1) -> fast_front(list)
    endif;

    ;;; increment ref count for new label clist
    clist -> source!INST_ARGS[_0];      ;;; insert new label
    _int(fast_front(clist)) -> _test;
    if _neg(_test) and not(_test _bitst _:M_LAB_REFMAX) then
        _pint(_test _add _:M_LAB_REF1) -> fast_front(clist)
    endif
enddefine;

define lconstant Optimise_clist_tl(clist, is_not);
    lvars instr, clist, next_clist = fast_back(clist), is_not, _op;
    until isvector(fast_front(next_clist) ->> instr) do
        returnif(_int(instr) _bitst _:M_LAB_REFCOUNT) (false);
        ;;; remove redundant label
        fast_back(next_clist) ->> next_clist -> fast_back(clist)
    enduntil;
    instr!INST_OP -> _op;
    if _op == I_NOT then
                ;;; Check for double negation, we could do better but
                ;;; this is very rare.
                returnif(is_not)(false);
        Optimise_clist_tl(next_clist, true)
    else
        if IS_UNOPT_JUMP(_op) ->> _op then
            ;;; unoptimised IF, BOOL etc
            Optimise_conditions(_op, next_clist)
        endif;
        instr!INST_OP -> _op;
        returnunless(IS_OPT_CONDITION(_op, instr)) (false);
        if is_not then
            if _op == I_IF_opt then
                not(instr!INST_ARGS[_1]) -> instr!INST_ARGS[_1]
            else
                if _op == I_BOOL_opt then I_NBOOL_opt else I_BOOL_opt endif
                        ->> _op -> instr!INST_OP
            endif;
            instr -> fast_front(clist);
            fast_back(next_clist) -> fast_back(clist);
            if clist == asm_clist then instr -> asm_instr endif
        endif;
        _op
    endif
enddefine;

define I_IF();
    Optimise_conditions(I_IF_opt, asm_clist);   ;;; becomes I_IF_opt
    chain(I_IF_opt)
enddefine;

define I_BOOL();
    Optimise_conditions(I_BOOL_opt, asm_clist);
    fast_chain(asm_instr!INST_OP)           ;;; becomes I_BOOL_opt or I_IF_opt
enddefine;

define I_NBOOL();
    Optimise_conditions(I_NBOOL_opt, asm_clist);
    fast_chain(asm_instr!INST_OP)           ;;; becomes I_?_opt
enddefine;

define I_NBOOL_opt();
    lvars instr = asm_instr, opnd = instr!INST_ARGS[_3];
    I_BOOL_opt -> instr!INST_OP;
    false -> instr!INST_ARGS[_3];
    chain([%if opnd then opnd endif,
            Cons_inst(I_CALLSUB, _not, 2),
            instr %], Replace_instr)
enddefine;

    /*  {I_CMP _routine operand1 operand2}
        call compare subroutine or optimise in-line with a following I_IF
    */
define I_CMP();
    lvars   instr = asm_instr, ifinstr, opnd1 = instr!INST_ARGS[_1],
            opnd2 = instr!INST_ARGS[_2], _op, _routine;
    ;;; test for whether can be optimised with a following IF
    if Optimise_clist_tl(asm_clist, false) == I_IF_opt then
        ;;; can do it -- replace with I_IF_CMP
        fast_destpair(fast_back(asm_clist))
                        -> (ifinstr, fast_back(asm_clist)); ;;; remove I_IF
        ;;; following is mainly for optimising compares on dlocal_context
        if opnd2 and opnd1
        and opnd2!INST_OP /== I_MOVE and opnd1!INST_OP /== I_MOVE then
            ;;; both operands constant
            instr!INST_ARGS[_0] -> _routine;
            if _routine(opnd2!INST_ARGS[_0], opnd1!INST_ARGS[_0])
                == ifinstr!INST_ARGS[_1] then
                ;;; will always jump
                Cons_inst(I_GOTO_opt, ifinstr!INST_ARGS[_0], I_BR, 3)
            else
                ;;; will never jump -- remove
                []
            endif
        else
            Cons_inst(explode(instr), ifinstr, 5) -> instr;
            I_IF_CMP -> instr!INST_OP;
            instr
        endif
    else
        ;;; else replace with subroutine call, with pushes for any args first
        [%  if opnd2 then opnd2 endif,
            if opnd1 then opnd1 endif,
            Cons_inst(I_CALLSUB, instr!INST_ARGS[_0], 2)
        %]
    endif;
    chain((), Replace_instr)
enddefine;

    /*  {I_TAG_TEST _routine operand}
        call tag-test subroutine or optimise in-line with a following I_IF
        (where _routine is _iscompound, _issimple or _isinteger)
    */
define I_TAG_TEST();
    lvars instr, ifinstr, opnd;
    ;;; test for whether can be optimised with a following IF
    if testdef I_IF_TAG
    and Optimise_clist_tl(asm_clist, false) == I_IF_opt then
        ;;; can do it -- replace with I_IF_TAG
        fast_destpair(fast_back(asm_clist))
                        -> (ifinstr, fast_back(asm_clist)); ;;; remove I_IF
        Cons_inst(explode(asm_instr), ifinstr, 4) -> instr;
        weakref I_IF_TAG -> instr!INST_OP;
        if instr!INST_ARGS[_0] == _iscompound then
            ;;; _iscompound = not(_issimple)
            _issimple -> instr!INST_ARGS[_0];
            not(ifinstr!INST_ARGS[_1]) -> ifinstr!INST_ARGS[_1]
        endif;
        instr
    else
        ;;; else replace with subroutine call, with push for an arg first
        asm_instr -> instr;
        [%  if instr!INST_ARGS[_1] ->> opnd then opnd endif,
            Cons_inst(I_CALLSUB, instr!INST_ARGS[_0], 2)
        %]
    endif;
    chain((), Replace_instr)
enddefine;

define I_NOT();
    lvars op;
    if Optimise_clist_tl(asm_clist, true) ->> op then
        fast_chain(op)
    else
        chain(Cons_inst(I_CALLSUB, _not, 2), Replace_instr)
    endif
enddefine;

define I_GOTO();
    Optimise_conditions(I_GOTO_opt, asm_clist);     ;;; becomes I_GOTO_opt
    chain(I_GOTO_opt)
enddefine;

define I_GOTO_opt();
    lvars instr = asm_instr;
    ;;; apply I_BR routine to label offset
    fast_apply(_int(fast_front(instr!INST_ARGS[_0])), _1, instr!INST_ARGS[_1])
enddefine;


define lconstant Chains_code(_routine);
    lvars _routine;
    ;;; swap return address of previous caller with chain routine address
    ;;; chain routine will reinstate this from chain_reg
    Cons_inst(I_MOVE_CALLER_RETURN, asm_cllr_return_id, ident chain_reg,
                                                                I_MOVE, 4),
    Cons_inst(I_MOVE_CALLER_RETURN, _routine, asm_cllr_return_id,
                                                            I_MOVEADDR, 4),
    Cons_goto(asm_exit_lab)
enddefine;

    ;;; {I_CHAINPS <move instr or false>}
define I_CHAINPS();
    lvars instr = asm_instr!INST_ARGS[_0];
    if _Nlocals _gr _2 or _asm_pdr_flags _bitst _:M_PD_PROC_DLEXPR_CODE then
        ;;; do it by I_CHAINS, after pushing the operand (if any)
        [% if instr then copy(instr) endif, Chains_code(_sysncchain) %]
    else
        ;;; move arg to chain_reg
        [%  if instr then
                Cons_inst(explode(instr), ident chain_reg, 3)
            else
                ;;; next dummy instruction is to stop things like I_FASTFIELD
                ;;; grabbing the following I_POP on the second pass (and thus
                ;;; planting different code than on the first pass).
                Cons_inst(identfn, 1),
                Cons_pop(ident chain_reg)
            endif
        %] nc_<>
        if asm_chainp_lab then
            ;;; other than first -- branch to code from first
            [% Cons_goto(asm_chainp_lab) %]
        else
            ;;; first occurrence -- plant unwind code with label
            Newlab_ref1([%  Cons_inst(I_UNWIND_SF, 1),
                            Cons_inst(I_CHAIN_REG, ident chain_reg, 2)
                        %]) ->> asm_chainp_lab
        endif
    endif;
    chain((), Replace_instr)
enddefine;

    ;;; {I_CHAINS}
define I_CHAINS();
    chain([% Chains_code(_syschain) %], Replace_instr)
enddefine;

    ;;; {I_FAST_+- <true if plus> <operand 1> <operand 2>}
define I_FAST_+-();
    lvars instr, opnd, destin = false, next_clist = fast_back(asm_clist);
    if testdef I_FAST_+-_any then
        ;;; just use that
        fast_chain(weakref I_FAST_+-_any ->> asm_instr!INST_OP)
    endif;

    ;;; see if can optimise a following pop
    fast_front(next_clist) -> instr;
    asm_instr!INST_ARGS[_2] -> opnd;
    if isvector(instr) and instr!INST_OP == I_POP then
        ;;; yes -- now see if it's the same as input arg2
        ;;; (i.e. that operand is being read then written)
        fast_back(next_clist) -> fast_back(asm_clist);  ;;; remove I_POP
        instr!INST_ARGS[_0] -> destin;
        if opnd and opnd!INST_OP == I_MOVE and opnd!INST_ARGS[_0] == destin
        then
            ;;; change it to an I_FAST_+-_2
            fast_chain(weakref I_FAST_+-_2 ->> asm_instr!INST_OP)
        endif
    elseunless opnd then
        ;;; change it to an I_FAST_+-_2 with stack in and out
        fast_chain(weakref I_FAST_+-_2 ->> asm_instr!INST_OP)
    endif;
    ;;; else make it an I_FAST_+-_3
    Cons_inst(explode(asm_instr), destin, 5) -> instr;
    weakref I_FAST_+-_3 -> instr!INST_OP;
    chain(instr, Replace_instr)
enddefine;

define I_GO_ON();
    lvars instr = asm_instr, p, lablist, elselab, base;
    ;;; deref labels
    instr!INST_ARGS[_0] -> lablist;
    fast_for p on lablist do
        Deref_lab(fast_front(p)) -> fast_front(p)
    endfast_for;
    if instr!INST_ARGS[_1] ->> elselab then
        Deref_lab(elselab) -> elselab
    endif;
    instr!INST_ARGS[_2] -> base;
    ;;; make it an I_SWITCH, plus an instruction after for when I_SWITCH falls
    ;;; thru with an out-of-range argument
    [%  if testdef I_SWITCH_base then
            Cons_inst(weakref I_SWITCH_base, lablist, elselab, false, base, 5)
        else
            if base /== 1 then
                Cons_inst(I_FAST_+-, true, Cons_inst(I_MOVENUM,1 fi_- base,2),
                                                            false, 4)
            endif;
            Cons_inst(weakref I_SWITCH, lablist, elselab, false, 4)
        endif;
        if elselab then
            ;;; else label
            Cons_goto(elselab)
        else
            Cons_inst(I_CALLABS, Go_on_outrange, 2)
        endif
    %];
    chain((), Replace_instr)
enddefine;

define I_DLOCAL_CONTEXT();
    ;;; should always have been substituted
    mishap(0, 'INVALID USE OF dlocal_context OR dlocal_process')
enddefine;

define I_LBLOCK();
    lvars   (, s_list, f_list, clos_inits) = explode(asm_instr), id,
            s_len = listlength(s_list), code;

    [%  ;;; add code for identifiers initialised to run-time idents
        if f_list /== [] then
            ;;; typed required, and maybe untyped
            Cons_pushq(f_list),                     ;;; typed template list
            Cons_pushq(listlength(f_list)+s_len),   ;;; total number required
            Cons_callabs(Cons_rt_idents),           ;;; cons pdr
            fast_for id in s_list <> rev(f_list) do
                Cons_inst(I_POPQ, id, 2)            ;;; pop them
            endfor
        elseif s_list /== [] then
            ;;; only untyped -- use quicker procedure
            Cons_pushq(s_len),                      ;;; number required
            Cons_callabs(Cons_rt_idents_untyped),   ;;; cons pdr
            fast_for id in s_list do
                Cons_inst(I_POPQ, id, 2)            ;;; pop them
            endfor
        endif;

        ;;; code to initialise local lvars to hold lexical closures
        fast_for id in clos_inits do
            Cons_inst(I_MOVEADDR, false, id, 3)
        endfor
    %] -> code;

    if code == [] then
        ;;; replace with dummy instruction to stop things like I_FASTFIELD
        ;;; grabbing a following I_POP on the second pass (and thus
        ;;; planting different code than on the first pass).
        Cons_inst(identfn, 1) -> code
    endif;
    chain(code, Replace_instr)
enddefine;

    /*  Turn a raw pointer on the stack into an exptr, either by
        consing a new one or using a fixed one.
        (Only planted by fields.p, which will ensure external_ptr_key
        is defined.)
    */
define I_MAKE_EXPTR();
    if asm_instr!INST_ARGS[_0] then
        ;;; cons a new one
        Cons_inst(I_CALLABS, weakref[external_ptr_key] Cons_extern_ptr, 2)
    else
        ;;; put in a fixed one (bit of a hack to use I_POP for this!)
        lvars an_exptr = writeable weakref[external_ptr_key] consexternal_ptr();
        [% Cons_inst(I_POP, an_exptr, 2), Cons_inst(I_MOVEQ, an_exptr, 2) %]
    endif;
    chain((), Replace_instr)
enddefine;



;;; --- Cons_procedure --------------------------------------------------

    ;;; Put a word in the procedure record (on final pass)
define Drop_word(_word);
    lvars _word;
    ;;; just increment except on last pass
    unless _asm_pass then
        _word -> _asm_drop_ptr!(w)++ -> _asm_drop_ptr
    endunless;
    @@(w){_asm_code_offset}++ -> _asm_code_offset
enddefine;

define Drop_I_code(asm_instr);
    dlocal asm_instr;
    fast_apply(asm_instr!INST_OP)
enddefine;

    /*  Mapping of identifiers to registers
    */
define Is_register(item);
    lvars item;
    if iscompound(item) and item!KEY == ident_key
    and not(item!ID_IDENTPROPS _bitst _:M_ID_LEX)
    and item!ID_PERM_FLAGS _bitst _:M_PERM_REGISTER then
        ;;; bit 0 on means this is a pop register
        ;;; rest of valof is implementation dependent, but is
        ;;; usually the register number
        fast_idval(item) fi_>> 1
    else
        false
    endif
enddefine;

    /*  Find the offset for a structure in the structure table
        and add it if not already a member of asm_struct_list
    */
define Structab_offset(item) -> _item_offset;
    lvars slist, item, _item_offset = _0;
    if asm_struct_list == [] then
        conspair(item, []) -> asm_struct_list
    endif;
    asm_struct_list -> slist;
    until fast_front(slist) == item do
        @@(w){_item_offset}++ -> _item_offset;
        if fast_back(slist) == [] then
            conspair(item, []) -> fast_back(slist);
            quitloop
        endif;
        fast_back(slist) -> slist
    enduntil
enddefine;


    /*  Translate a structure operand on the first pass: returns either
            (a) A register identifier
            (b) A positive pop integer offset into the structure table
            (c) A negative pop integer coding an offset in the stack frame
        (or just returns the given value if this is simple or in the system)
    */
define Trans_structure(structure);
    lvars structure;
    if issimple(structure) or structure <@(w) _system_end then
        structure
    elseif structure!KEY == ident_key
    and structure!ID_IDENTPROPS _bitst _:M_ID_LEX_TOKEN then
        ;;; lvar token identifier
        if issimple(fast_idval(structure))
        or structure!ID_LEX_FLAGS _bitst _:M_LEX_USE_REG then
            ;;; valof already a popint offset to something, or a reg identifier
            fast_idval(structure)
        else
            ;;; non-local lvar accessing a dynamic in the valof
            ;;; get pop integer offset and replace for other occurrences
            _pint(Structab_offset(fast_idval(structure))) ->> fast_idval(structure)
        endif
    else
        ;;; structure in literal table
        ;;; make offset a (+ve) pop integer
        _pint(Structab_offset(structure))
    endif
enddefine;


    /*  Find the offset for a literal in the nonpop literal table
        and add it if not already a member of asm_nplit_list. The
        offset has the (high) bit _NONPOP_LIT set to distinguish it from
        struct table offsets returned by Structab_offset.
    */
define Np_literal_offset(_item) /* -> offset */;
    lvars l = asm_nplit_list, npfp, lrec, _item, _offs = _0;

    lconstant
        npfront_pair_key = struct KEY_GC =>>
    {%  _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _0,                     ;;; K_FLAGS
        _:GCTYPE_FULL2ND_REC2,  ;;; K_GC_TYPE
        Rec2_getsize,           ;;; K_GET_SIZE
    %};

    if l == [] then
        false -> lrec
    else
        repeat
            returnif(l!FIRST == _item) (_offs _biset _NONPOP_LIT);
            @@(w){_offs}++ -> _offs;
            if l!SECOND == [] then
                l -> lrec;
                quitloop
            else
                l!SECOND -> l
            endif
        endrepeat
    endif;

    ;;; not found -- add new entry using an 'npfront_pair', i.e. like a pair
    ;;; but whose front is a nonpop value.
    _free_npfront_pairs -> npfp;        ;;; freelist cleared by GC
    if issimple(npfp) then
        Get_store(@@(struct POPREC2)++) -> npfp;
        npfront_pair_key -> npfp!KEY
    else
        npfp!SECOND -> _free_npfront_pairs
    endif;
    _item -> npfp!FIRST;
    [] -> npfp!SECOND;
    if lrec then npfp -> lrec!SECOND else npfp -> asm_nplit_list endif;

    ;;; update the size of the literal table (this includes 2 extra words
    ;;; -- see Get_procedure below)
    _offs _add @@(w)[_3] -> _asm_nplit_size;

    _offs _biset _NONPOP_LIT
enddefine;



    /*  Process instructions - each routine generates code
        thru Drop_ procedures which update the running offset but only plant
        the code on the final pass
    */
define Code_pass(_asm_pass, asm_clist) -> _asm_code_offset;
    lvars   instr, _op, _movop = false;
    dlocal  asm_clist, asm_instr, _asm_pass,
            _asm_code_offset = _0,
            _asm_offset_diff = _0;

    _CHECKINTERRUPT;
    if _asm_pass == 0 then I_MOVE -> _movop endif;

    until asm_clist == [] do
        fast_front(asm_clist) -> instr;     ;;; next instruction
        if isvector(instr) then
            instr -> asm_instr;
            instr!INST_OP -> _op;
            if _op == _movop and instr!V_LENGTH == _2
            and Optimise_clist_tl(asm_clist, false) then
                ;;; make it the operand of a following IF or BOOL
                fast_destpair(fast_back(asm_clist))
                                -> (instr, fast_back(asm_clist));
                asm_instr -> instr!INST_ARGS[_3];
                Replace_instr(instr)
            else
                fast_apply(_op)
            endif
        else
            I_LABEL()
        endif;
        ;;; must be done afterwards in case the instruction changes asm_clist
        fast_back(asm_clist) -> asm_clist
    enduntil
enddefine;

    ;;; Get a procedure record and fill in enough of the header to make safe
    ;;; for GC -- called from Do_consprocedure.
define Get_procedure(_size, _reg_spec) -> pdr;
    lvars pdr, l, _size, _reg_spec;
    _CHECKINTERRUPT;
    Get_store(_size) -> pdr;
    ##(w){_size} ->> _size -> pdr!PD_LENGTH;
    unless pdr!PD_LENGTH == _size then
        pdr -> Get_store();     ;;; junk it
        mishap(0, 'PROCEDURE EXCEEDS MAXIMUM ALLOWABLE SIZE')
    endunless;

    procedure_key   -> pdr!KEY;
    false           -> pdr!PD_PROPS;
    false           -> pdr!PD_UPDATER;
    _0              -> pdr!PD_FLAGS;
    _reg_spec       -> pdr!PD_REGMASK;

    ;;; must disable interrupts and inhibit GCs before setting up
    ;;; _asm_drop_ptr  (_disable and _inhibit_gc are dlocal to
    ;;; Cons_procedure)
    _DISABLE_ALL -> _disable;
    true -> _inhibit_gc;

    ;;; set up _asm_drop_ptr for final code generation pass,
    ;;; and fill in PD_EXECUTE and struct and np literal tables
    pdr@PD_TABLE@(w->code) -> _asm_drop_ptr; ;;; start of structure table
    false -> _asm_pass;

    ;;; generate structure table
    applist(asm_struct_list, Drop_word);

    ;;; generate nonpop literal table (if there is one)
    if (asm_nplit_list ->> l) /== [] then
        until l == [] do
            Drop_word(l!FIRST);
            l!SECOND -> l
        enduntil;
        ;;; fix up the struct table by adding two words to it
        Drop_word(_asm_nplit_size);
        ;;; next value appearing in the word before PD_EXECUTE tells GC that
        ;;; previous word contains a word offset size to ignore at the end
        ;;; of PD_TABLE (i.e. _asm_nplit_size)
        Drop_word($-Sys$-Gc$-pd_table_noscan_region)
    endif;

    _asm_drop_ptr -> pdr!PD_EXECUTE         ;;; start of instructions
enddefine;

    /*  Allocate lvars to registers or stack frame cells, setting the
        idval of each token identifier to its translation.
    */
define lconstant Alloc_lvars(l_list, _nstk, _nreg, reglist) -> _mstk -> _mreg;
    lvars   id, l_list, reglist, _flags, _offs, _nstk, _nreg, _x,
            _mreg = _nreg, _mstk = _nstk;
    fast_for id in l_list do
        if isident(id) then
            id!ID_LEX_FLAGS -> _flags;
            ;;; allocate to a reg if any left
            if reglist == []
            or _flags _bitst (_:M_LEX_DONT_USE_REG _biset _:M_LEX_RTID_ACCESS)
            then
                ;;; allocate to the stack -- put the negated stack offset
                ;;; as a pop integer into the idval of the identifier
                ;;; if RTID_ACCESS is set, the lvar will contain an ident and
                ;;; the idval of this is what we access. We code this by
                ;;; setting bit 0 of the offset
                _flags _biclear _:M_LEX_USE_REG -> id!ID_LEX_FLAGS;
                _shift(_negate(@@SF_LOCALS[_nstk]), _1) -> _offs;
                if _flags _bitst _:M_LEX_RTID_ACCESS then
                    _offs _biset _1 -> _offs
                endif;
                _nstk _add _1 ->> _nstk -> _mstk;
                _pint(_offs)
            else
                ;;; allocate to a reg
                _flags _biset _:M_LEX_USE_REG -> id!ID_LEX_FLAGS;
                _nreg _add _1 ->> _nreg -> _mreg;
                fast_destpair(reglist) -> reglist
            endif -> fast_idval(id)
        else
            ;;; nested lblock l_list (follows all idents)
            if (Alloc_lvars(id, _nstk, _nreg, reglist) ->> _x) _gr _mstk then
                _x -> _mstk
            endif;
            if (->> _x) _gr _mreg then _x -> _mreg endif
        endif
    endfor
enddefine;


    /*  Stack data words to go into the structure table for dlocal active
        variables (will be used by Dlocal_frame_offset)
    */
define lconstant Stack_dlocal_actid_data(actid_dlexprs);
    lvars dlx, actid_dlexprs, v, _i, _index_data, _index, _stkptr;
    repeat
        0;                  ;;; dummy where index_data will go
        _user_sp() -> _stkptr;
        _3 -> _i;           ;;; skip count bits
        _0 -> _index_data;
        repeat
            fast_destpair(actid_dlexprs) -> (dlx, actid_dlexprs);
            dlx!DLX_VARS -> v;
            if v == [] then
                _1          ;;; dummy for multiplicity 0
            else
                fast_front(v) -> v;
                ;;; word index into the stack frame of the first save lvar
                ##(w){ _negate(_shift(_int(fast_idval(v)), _-1)) }
            endif -> _index;
            _shift(_index, _i) _biset _index_data _add _1 -> _index_data;
            _i _add _8 -> _i;
            dlx!DLX_ACTID;          ;;; stack the active identifier
            quitif(actid_dlexprs == []) (2);
            quitif(_i _gr _:POPINT_BITS _sub _8)
        endrepeat;
        _pint(_index_data) -> _stkptr!(w)   ;;; replace the dummy
    endrepeat;
    ;;; replace the dummy, but this time with -1 in the top bits to indicate
    ;;; this is the last index word
    _pint(_shift(_-1, _i) _biset _index_data) -> _stkptr!(w)
enddefine;


define Cons_procedure(codelist, d_locals, nargs, props, pop_l_locals,
                        nonpop_l_locals, plab_list, asm_exit_lab,
                        asm_cllr_return_id, lit_cache_list, actid_dlexprs,
                        _asm_pdr_flags)
                    -> pdr;

    lvars   id, pdr, l, d_locals, pop_l_locals, nonpop_l_locals, props, nargs,
            codelist, plab, lab, plab_list, lit_pair, lit_cache_list,
            actid_dlexprs, rem_np_regs, rem_p_regs,
            _nreg, _Nnplocals, _len, _flags2 = _0;

    dlvars  reg_locals;

    dlocal  _disable, _inhibit_gc,
            ;;; some of these are used non-locally by Do_consprocedure in ass.p
            asm_struct_list = [], asm_nplit_list = [],
            _asm_nplit_size = _0,
            asm_exit_lab, asm_chainp_lab = false,
            asm_cllr_return_id, _asm_pdr_flags,
            _Nlocals, _Nreg, _Npopreg, _Nstkvars, _Npopstkvars, _Nframewords;

    define lconstant Add_structab(); Structab_offset() -> enddefine;

    _0 ->> _Nstkvars -> _Nnplocals;

    ;;; add always-local registers at start of asm_pop/nonpop_registers
    dest(asm_pop_registers) -> rem_p_regs -> reg_locals;
    _int(listlength(reg_locals)) -> _Npopreg;
    (dest(asm_nonpop_registers) -> rem_np_regs) <> reg_locals -> reg_locals;

    if _asm_pdr_flags _bitst _:M_PD_PLOG_CHOICE then
        ;;; Prolog procedure -- Allow for the 3 special prolog nonpop lvars
        ;;; (SF_PLGSV_NEXT_VAR, SF_PLGSV_CONTN_TOP, SF_PLGSV_TRAIL_SP).
        ;;; Also the special dlocal _plog_save_contn_sp .
#_IF DEF SPARC
        ;;; Add dlocals to asm_struct_list -- _plog_save_contn_sp is
        ;;; nonpop, so must come FIRST
        Add_structab(ident weakref _plog_save_contn_sp);
        applist(d_locals, Add_structab);
        ;;; The 3 special lvars are the first 3 nonpop registers
        [% dest(dest(dest(rem_np_regs))) -> rem_np_regs %]
                                            nc_<> reg_locals -> reg_locals;
#_ELSE
        ;;; Add dlocals to asm_struct_list -- _plog_save_contn_sp is
        ;;; nonpop, so must come LAST
        applist(d_locals, Add_structab);
        Add_structab(ident weakref _plog_save_contn_sp);
        ;;; The 3 special lvars are on-stack, so skip 3 stack frame slots
        _3 -> _Nstkvars;
#_ENDIF
        _1 -> _Nnplocals            ;;; record 1 nonpop dynamic local
    else
        ;;; Add dynamic locals to asm_struct_list
        applist(d_locals, Add_structab)
    endif;

    ;;; deal with lvars, allocating as many as possible to registers

    define lconstant Add_nregs(_n, reglist) -> reglist;
        lvars reglist, _n;
        until _zero(_n) do
            (fast_destpair(reglist) -> reglist) :: reg_locals -> reg_locals;
            _n _sub _1 -> _n
        enduntil
    enddefine;

    ;;; do nonpop lvars first
    Alloc_lvars(nonpop_l_locals, _Nstkvars, _0, rem_np_regs)
                                                -> _Nstkvars -> _nreg;
    Add_nregs(_nreg, rem_np_regs) -> rem_np_regs;

    ;;; then pop lvars
    dup(Alloc_lvars(pop_l_locals, _Nstkvars, _0, rem_p_regs)) _sub _Nstkvars
                                -> _Npopstkvars -> _Nstkvars -> _nreg;
    Add_nregs(_nreg, rem_p_regs) -> rem_p_regs;
    _Npopreg _add _nreg -> _Npopreg;

    ;;; allocate literals given to any remaining nonpop regs
    ;;; (currently only used for I_CALLSUB_REG with prolog subroutines)
    while lit_cache_list /== [] do
        quitif(rem_np_regs == []);
        fast_destpair(rem_np_regs) -> rem_np_regs -> id;
        quitunless(Is_address_reg(id));
        fast_destpair(lit_cache_list) -> lit_cache_list -> lit_pair;
        ;;; front of lit_pair is routine name -- add instr to move to reg
        Cons_inst(I_MOVEADDR, fast_front(lit_pair), id, 3) :: codelist
                                                            -> codelist;
        id -> fast_front(lit_pair);     ;;; make reg be used for calls
        id :: reg_locals -> reg_locals
    endwhile;

    ;;; total number of registers
    _int(listlength(reg_locals)) -> _Nreg;

    ;;; total number of dlocals
    _int(listlength(asm_struct_list)) -> _Nlocals;

    ;;; values for procedure header
    lvars _pd_num_pstk_vars, _pd_gc_offset_len, _pd_gc_scan_len;

#_IF DEF SPARC

    ;;; PD_NUM_PSTK_VARS contains the number of pop registers used
    _Npopreg -> _pd_num_pstk_vars;

    ##SF_LOCALS _add _Nstkvars -> _len;     ;;; length to start of dlocals

    ;;; PD_GC_OFFSET_LEN is the length to the first pop onstack lvar
    _len _sub _Npopstkvars -> _pd_gc_offset_len;

    ;;; PD_GC_SCAN_LEN is the number of pop onstack lvars + pop dlocals
    _Npopstkvars _add _Nlocals _sub _Nnplocals -> _pd_gc_scan_len;

    ;;; length of stack frame in words
    _len _add _Nlocals -> _Nframewords;

#_ELSE  ;;; not SPARC

    ;;; PD_NUM_PSTK_VARS contains the number of pop onstack lvars
    _Npopstkvars -> _pd_num_pstk_vars;

    ##SF_LOCALS _add _Nstkvars -> _len;     ;;; length to start of dlocals

    ;;; PD_GC_OFFSET_LEN is the length to the first pop dlocal
    _len _add _Nnplocals -> _pd_gc_offset_len;

    ;;; PD_GC_SCAN_LEN is the number of pop dlocals + pop registers
    _Nlocals _sub _Nnplocals _add _Npopreg -> _pd_gc_scan_len;

    ;;; length of stack frame in words
    _len _add _Nlocals _add _Nreg _sub ##SF_RETURN_ADDR -> _Nframewords;

#_ENDIF

#_IF DEF STACK_ALIGN_BITS
    ;;; stack frame must be aligned on a multi-word boundary
    lconstant macro STACK_ALIGN_WORDS = STACK_ALIGN_BITS div WORD_BITS;

    define lconstant Fix_offsets(l_list, _offs);
        lvars id, l_list, _offs;
        fast_for id in l_list do
            if isident(id) then
                if issimple(fast_idval(id)) then
                    _pint( _int(fast_idval(id)) _sub _shift(_offs, _1) )
                                                        -> fast_idval(id);
                endif;
            else
                Fix_offsets(id, _offs);
            endif;
        endfor;
    enddefine;

    lvars (_pad,) = _Nframewords _div _:STACK_ALIGN_WORDS;
    unless _zero(_pad) then
        _:STACK_ALIGN_WORDS _sub _pad -> _pad;
        ;;; pad out the stack frame by adding that number of non-pop
        ;;; on-stack lvars
        _Nstkvars _add _pad -> _Nstkvars;
        ;;; adjust values for procedure header
        _Nframewords _add _pad -> _Nframewords;
        _pd_gc_offset_len _add _pad -> _pd_gc_offset_len;
        ;;; adjust offsets for pop on-stack lvars
        unless _zero(_Npopstkvars) then
            Fix_offsets(pop_l_locals, @@(csword)[_pad]);
        endunless;
    endunless;
#_ENDIF

#_IF not(DEF SPARC)
    ;;; set up asm_cllr_return_id as a stack var
    _pint(_shift(_negate(@@SF_RETURN_ADDR[_Nframewords]), _1))
                                -> fast_idval(asm_cllr_return_id);
#_ENDIF

    ;;; set up dlocal active var data in the structure table (used by
    ;;; Dlocal_frame_offset)
    if actid_dlexprs /== [] then
        asm_struct_list nc_<> [% Stack_dlocal_actid_data(actid_dlexprs) %]
                        -> asm_struct_list;
        _flags2 _biset _:M_PD2_HAS_DLOCAL_ACTIVE -> _flags2
    endif;

    Incr_lab_refcount(asm_exit_lab);

        if _Nframewords _gr _:MAX_STACK_FRAME_SIZE then
          mishap(0, 'PROCEDURE STACK FRAME TOO LARGE (too many lvars?)');
        endif;
    ;;; first instruction creates stack frame on entry
    Cons_inst(I_CREATE_SF, 1) :: codelist -> codelist;

    ;;; assemble the procedure
    Do_consprocedure(codelist, reg_locals) -> pdr;

    ;;; fill in rest of header
    _asm_pdr_flags                      -> pdr!PD_FLAGS;
    _flags2                             -> pdr!PD_FLAGS2;
    _Nstkvars                           -> pdr!PD_NUM_STK_VARS;
    _pd_num_pstk_vars                   -> pdr!PD_NUM_PSTK_VARS;
    _Nlocals                            -> pdr!PD_NLOCALS;
    _Nframewords                        -> pdr!PD_FRAME_LEN;
    _pd_gc_scan_len                     -> pdr!PD_GC_SCAN_LEN;
    _pd_gc_offset_len                   -> pdr!PD_GC_OFFSET_LEN;

    ;;; address of exit code
    pdr!PD_EXECUTE@(code){_int(fast_front(asm_exit_lab))}
                                        -> pdr!PD_EXIT;
    props                               -> pdr!PD_PROPS;
    _int(nargs)                         -> pdr!PD_NARGS;

#_IF DEF CACHEFLUSH
    Flush_procedure(pdr);
#_ENDIF

    ;;; Deal with procedure labels. Each label pair in the codelist
    ;;; now contains in its front the (popint) code offset from PD_EXECUTE
    ;;; for that label. For each procedure label record, fill in the label
    ;;; offset in PLAB_OFFSET.
    fast_for plab in plab_list do
        _int(fast_front(plab!PLAB_LABEL)) -> plab!PLAB_OFFSET;
        ;;; label is no longer required -- replace PLAB_LABEL with false,
        ;;; which says that PLAB_OFFSET contains an offset (Popc generates
        ;;; true for this, meaning PLAB_OFFSET is an absolute address)
        false -> plab!PLAB_LABEL
    endfor;

    sys_grbg_list(asm_struct_list);
    Garbage_codelist(codelist);

    ;;; garbage any 'npfront_pairs' used for nonpop literals
    if (asm_nplit_list ->> l) /== [] then
        until l!SECOND == [] do l!SECOND -> l enduntil;
        _free_npfront_pairs -> l!SECOND;
        asm_nplit_list -> _free_npfront_pairs
    endif
enddefine;

endsection;     /* $-Sys$-Vm */



/* --- Revision History ---------------------------------------------------
--- David Young, Oct  8 2003
        Fixed bug in Optimise_conditions which caused a sequence of I_NOTs
        followed by an I_GOTO to be incorrectly ignored. This was
        manifested for example by
            if true then not(true or true) else endif
        leaving <true> on the stack. The fix involves the variable
        _no_nots.
--- John Gibson, Oct 17 1996
        # New I_GO_ON includes base integer and uses whichever of I_SWITCH
          or I_SWITCH_base is present.
        # I_FAST_+- uses I_FAST_+-_any if present instead of I_FAST_+-_2/3.
--- John Gibson, Sep 27 1996
        Added I_TAG_TEST, and improved optimising of conditions by
        using new label reference count to remove redundant labels.
--- John Gibson, May 27 1995
        Changed Cons_procedure to take a list of dlocal active vars and
        use it to generate data in the procedure header for use by
        Dlocal_frame_offset.
--- John Gibson, Apr 25 1995
        Added nonpop literal table mechanisms
--- John Gibson, Jan 27 1995
        Made I_LBLOCK replace itself with a dummy instruction when generating
        no code (to stop I_FASTFIELD etc grabbing a following I_POP on the
        second pass)
--- John Gibson, Oct 18 1994
        free*pairs -> _free_pairs
--- John Gibson, Sep 19 1994
        Implemented chains of free instruction vectors upto length 7 in
        _freevectab, and made Cons_procedure garbage the codelist and
        instruction vectors directly (was previously done in vm_asm.p)
--- John Gibson, Sep  6 1994
        Renamed _pd_f*rame_len lvar in Cons_procedure as perm var _Nframewords
        and made it dlocal.
--- Robert John Duncan, Mar 23 1994
        Changed Cons_procedure to align stack frames on a boundary
        specified by STACK_ALIGN_BITS (if defined).
--- John Gibson, May  4 1993
        Moved in I_ procedures from vm_optim.p and Cons_ instruction
        procedures from vm_asm.p
--- John Gibson, Aug 29 1992
        Made _inhibit_gc dlocal to Cons_procedure, and made Get_procedure
        assign true to it before last pass.
--- Robert John Duncan, Feb 11 1991
        Added cache flush
--- John Gibson, Feb  5 1991
        Added interrupt check in -Code_pass-
--- John Gibson, Dec  7 1989
        Changes for new pop pointers
--- John Gibson, Jun  4 1989
        Removed pop_optimise
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, Apr 30 1989
        Put into section $-Sys$-Vm.
--- John Gibson, Nov 23 1988
        Changes for lexical blocks.
--- John Gibson, Oct 21 1988
        Added Sys$-Vm$-Deref_lab(lab)
--- John Gibson, Jul 26 1988
        Register caching of prolog subroutines now done inside
        -Cons_procedure-.
        Also other changes for SPARC.
--- John Gibson, Jun  7 1988
        Made -Code_pass- call I_LABEL for a label
--- John Gibson, May 22 1988
        Altered -Trans_structure- to return its argument if this is simple
        or a structure in the system
 */
