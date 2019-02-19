/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/vm_optim.p
 > Purpose:         Optimising calls to system procedures
 > Author:          John Gibson (see revisions)
 */

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'vmdefs.ph'

global constant
        procedure (prolog_newvar, prolog_deref, fast_prolog_functor,
        prolog_assign, fast_prolog_arg,
        appproperty, fast_appproperty, maplist, lisp_true,
        lisp$-Lisp_setstacklength, Sys$- +_1, Sys$- -_1,
        ),
    ;

section $-Sys$-Vm;

global constant
        procedure (I_LISP_TRUE, I_FASTFIELD, I_UFASTFIELD,
        I_FASTSUBV, I_UFASTSUBV, I_CMP, I_FAST_+-,
        I_STACKLENGTH, I_SETSTACKLENGTH, I_DLOCAL_CONTEXT, rI_CHAINQ_LEX,
        I_CALLPS, I_UCALLPS, I_TAG_TEST,
        Unpush_lex, Opt_plog_assign
        ),
    ;

endsection;


;;; ----------------------------------------------------------------------

section $-Sys$-Vm => dlocal_context, dlocal_process;

define Get_operand();
    lvars instr;
    if _simstack _bitst _S_XPUSH
    and (fast_front(vm_code_end) ->> instr)!INST_OP /== rI_MOVEQ_LEX then
        instr
    else
        false
    endif
enddefine;

define lconstant Opt_chain(fast);
    lvars instr, fast;
    unless _simstack _lt _S_ONE and pop_debugging then
        if _simstack _bitst _S_XPUSH
        and (fast_front(vm_code_end) ->> instr)!INST_OP == rI_MOVEQ_LEX then
            rI_CHAINQ_LEX -> instr!INST_OP
        elseif fast or _simstack _bimask _S_TYPE == _S_PDR then
            if Get_operand() ->> instr then
                Unplant(true)       ;;; unplant the push
            endif;
            Plant(I_CHAINPS, instr, 2)
        else
            Plant(I_CHAINS, 1)
        endif;
        true
    else
        false
    endunless;
    _S_NONE -> _simstack
enddefine;

define lconstant Opt_fast_apply(upd);
    lvars instr, upd;
    unless _simstack _lt _S_ONE and pop_debugging then
        if _simstack _bitst _S_XPUSH
        and (fast_front(vm_code_end) ->> instr)!INST_OP == I_MOVE then
            if upd then I_UCALLP else I_CALLP endif -> instr!INST_OP
        else
            Plant(if upd then I_UCALLPS else I_CALLPS endif, 1)
        endif;
        true
    else
        false
    endunless;
    _S_NONE -> _simstack
enddefine;

define lconstant Opt_fast_field(fieldoff);
    lvars fieldoff, instr;
    ;;; fieldoff is false for fast_destpair
    unless _simstack _lt _S_ONE and pop_debugging then
        if Get_operand() ->> instr then
            if fieldoff == _pint(@@ID_VALOF) and instr!INST_OP == I_MOVEQ then
                I_MOVE -> instr!INST_OP;
                return(true)                ;;; don't alter _simstack
            else
                Unplant(false)              ;;; unplant the push
            endif
        endif;
        Plant(I_FASTFIELD, fieldoff, instr, 3);
        true
    else
        false
    endunless;
    _REP_ANY_simstack -> _simstack;
    unless fieldoff then
        _PUSH_ANY_simstack -> _simstack
    endunless
enddefine;

define lconstant Opt_ufast_field(fieldoff);
    lvars fieldoff, instr;
    unless _simstack _lt _S_TWO and pop_debugging then
        if Get_operand() ->> instr then
            if fieldoff == _pint(@@ID_VALOF) and instr!INST_OP == I_MOVEQ then
                I_POP -> instr!INST_OP
            else
                Cons_inst(I_UFASTFIELD, fieldoff, instr, 3)
                                    -> fast_front(vm_code_end)
            endif
        else
            Plant(I_UFASTFIELD, fieldoff, false, 3)
        endif;
        true
    else
        false
    endunless;
    _POP2_simstack -> _simstack
enddefine;

define lconstant Do_fast_subv(op, _base);
    lvars instr, op, _base, _subs;
    if Get_operand() ->> instr then
        ;;; see if can pull in an explicit subscript
        Unplant(true);
        if _simstack _bitst _S_XPUSH
        and isinteger(fast_front(vm_code_end)!INST_ARGS[_0] ->> _subs) then
            ;;; yes -- must be an I_MOVENUM for an integer.
            ;;; turn it into an I_FASTFIELD
            Unplant(false);
            _int(_subs) _sub _1 -> _subs;
            _pint(_int(_base) _add @@(w)[_subs]) -> _base;
            if op == I_FASTSUBV then I_FASTFIELD else I_UFASTFIELD endif -> op
        endif
    else
        _POP_simstack -> _simstack
    endif;
    Plant(op, _base, instr, 3)
enddefine;

define lconstant Opt_fast_subv(_base);
    lvars _base;
    unless _simstack _lt _S_TWO and pop_debugging then
        Do_fast_subv(I_FASTSUBV, _base);
        true
    else
        _POP_simstack -> _simstack;
        false
    endunless;
    _REP_ANY_simstack -> _simstack
enddefine;

define lconstant Opt_ufast_subv(_base);
    lvars _base;
    unless _simstack _lt _S_THREE and pop_debugging then
        Do_fast_subv(I_UFASTSUBV, _base);
        true
    else
        _POP_simstack -> _simstack;
        false
    endunless;
    _POP2_simstack -> _simstack
enddefine;

define lconstant Opt_lisp_true();
    -> ;                ;;; erase dummy arg
    Plant(I_LISP_TRUE, 1);
    _REP_ANY_simstack -> _simstack;
    true
enddefine;

define lconstant Get_operand_2() -> instr2 -> instr1;
    lvars instr1, instr2 = false;
    if Get_operand() ->> instr1 then
        Unplant(true);              ;;; pop _simstack
        ;;; see if can pull in a second operand
        if Get_operand() ->> instr2 then
            Unplant(true)           ;;; pop _simstack
        else
            _POP_simstack -> _simstack
        endif
    else
        _POP2_simstack -> _simstack
    endif
enddefine;

define lconstant Opt_cmp_op(_routine);
    lvars instr1, instr, instr2, _routine;
    unless _simstack _lt _S_TWO and pop_debugging then
        Get_operand_2() -> instr2 -> instr1;
        ;;; optimise lisp_true() {/}== []
        fast_front(vm_code_end) -> instr;
        if isvector(instr) and instr!INST_OP == I_LISP_TRUE
        and (_routine == nonop _eq or _routine == nonop _neq)
        and instr1 and not(instr2)
        and instr1!INST_OP == I_MOVEADDR and instr1!INST_ARGS[_0] == []
        then
            ;;; remove I_LISP_TRUE (_simstack already popped)
            Unplant(false);
            ;;; lisp_true()==[] -> I_NOT, lisp_true()/==[] -> no code
            if _routine == nonop _eq then Plant(I_NOT, 1) endif
        else
            Plant(I_CMP, _routine, instr1, instr2, 4)
        endif;
        true
    else
        _POP2_simstack -> _simstack;
        false
    endunless;
    _PUSH_ANY_simstack -> _simstack
enddefine;

define lconstant Opt_tag_test(_routine);
    lvars instr, _routine;
    unless _simstack _lt _S_ONE and pop_debugging then
        if Get_operand() ->> instr then Unplant(false) endif;
        Plant(I_TAG_TEST, _routine, instr, 3);
        true
    else
        false
    endunless;
    _REP_ANY_simstack -> _simstack
enddefine;

define lconstant Opt_fi_+-(plus);
    lvars plus, instr;
    unless _simstack _lt _S_TWO and pop_debugging then
        Plant(I_FAST_+-, plus, Get_operand_2(), 4);
        true
    else
        _POP2_simstack -> _simstack;
        false
    endunless;
    _PUSH_ANY_simstack -> _simstack
enddefine;

define lconstant Opt_dup(dummy);
    lvars dummy, _type;
    unless _simstack _lt _S_ONE and pop_debugging then
        Plant(I_MOVES, 1);
        true
    else
        false
    endunless;
    if _zero(_simstack _bimask _S_TYPE ->> _type) then _S_ANY -> _type endif;
    _PUSH_simstack _biset _type -> _simstack
enddefine;

define lconstant Opt_not(dummy);
    lvars dummy;
    Plant(I_NOT, 1);
    _REP_ANY_simstack -> _simstack;
    true
enddefine;

define lconstant Opt_+-_1(+-_1_pdr);
    lvars instr, +-_1_pdr;
    if _simstack _bitst _S_XPUSH
    and (fast_front(vm_code_end) ->> instr)!INST_OP == I_MOVENUM
    and instr!INST_ARGS[_0] == 1
    then
        Unplant(true);
        Plant(I_CALLABS, +-_1_pdr, 2);
        true
    else
        _POP_simstack -> _simstack;
        false
    endif;
    _REP_ANY_simstack -> _simstack
enddefine;

define lconstant Opt_sr_0a1r(_routine);
    lvars _routine;
    Plant(I_CALLSUB, _routine, 2);
    _PUSH_ANY_simstack -> _simstack;
    true
enddefine;

define lconstant Opt_sr_1a1r(_routine);
    lvars _routine;
    unless _simstack _lt _S_ONE and pop_debugging then
        Plant(I_CALLSUB, _routine, 2);
        true
    else
        false
    endunless;
    _REP_ANY_simstack -> _simstack
enddefine;

define lconstant Opt_sr_1a3r(_routine);
    lvars _routine _sm;
    unless _simstack _lt _S_ONE and pop_debugging then
        Plant(I_CALLSUB, _routine, 2);
        true
    else
        false
    endunless;
    _REP_ANY_simstack -> _simstack;
    _PUSH_ANY_simstack -> _simstack
enddefine;

define lconstant Opt_sr_2a1r(_routine);
    lvars _routine;
    unless _simstack _lt _S_TWO and pop_debugging then
        Plant(I_CALLSUB, _routine, 2);
        true
    else
        false
    endunless;
    _POP_simstack -> _simstack;
    _REP_ANY_simstack -> _simstack
enddefine;

define lconstant Opt_sr_2a2r(_routine);
    lvars _routine;
    unless _simstack _lt _S_TWO and pop_debugging then
        Plant(I_CALLSUB, _routine, 2);
        true
    else
        false
    endunless;
    _POP2_simstack -> _simstack;
    _PUSH_ANY_simstack -> _simstack;
    _PUSH_ANY_simstack -> _simstack
enddefine;

define lconstant Opt_sr_3a0r(_routine);
    lvars _routine;
    unless _simstack _lt _S_THREE and pop_debugging then
        Plant(I_CALLSUB, _routine, 2);
        true
    else
        false
    endunless;
    _POP3_simstack -> _simstack
enddefine;

define lconstant Opt_stacklength();
    -> ;            ;;; erase dummy arg
    Plant(I_STACKLENGTH, 1);
    _PUSH_ANY_simstack -> _simstack;
    true
enddefine;

define lconstant Opt_setstacklength() with_nargs 1;
    lvars instr nr sl;
    ->;             ;;; erase dummy arg
    unless _simstack _lt _S_TWO and pop_debugging then
        false ->> nr -> sl;
        unless pop_debugging then
            if _simstack _bitst _S_XPUSH
            and (fast_front(vm_code_end) ->> instr)!INST_OP == I_MOVENUM then
                instr!INST_ARGS[_0] -> nr;
                Unplant(true);
                if _simstack _bitst _S_XPUSH then
                    fast_front(vm_code_end) -> sl;
                    Unplant(true)
                endif;
            endif;
        endunless;
        Plant(I_SETSTACKLENGTH, sl, nr, 3);
        true
    else
        false
    endunless;
    _S_NONE -> _simstack
enddefine;

    ;;; for procedures like applist, appdata, etc, 'unpush' a push
    ;;; of a preceding lexical constant.
define lconstant Opt_try_unpush_lex();
    lvars instr;
    -> ;                ;;; erase dummy arg
    if _simstack _bitst _S_XPUSH then
        fast_front(vm_code_end) -> instr;
        if instr!INST_OP == rI_MOVEQ_LEX then
            Unpush_lex(instr!INST_ARGS[_0])
        endif
    endif;
    _S_NONE -> _simstack;
    ;;; return false since no code planted
    false
enddefine;


    ;;; Valid accesses to these are ALWAYS optimised to I_DLOCAL_CONTEXT.
    ;;; These procedures return <false> only so that -idval- and -valof-
    ;;; don't mishap with them.
define active dlocal_context; false enddefine;
define active dlocal_process; false enddefine;

define lconstant Opt_dlocal_context(arg);
    lvars arg;
    Plant(I_DLOCAL_CONTEXT, arg, 2);        ;;; recognised in vmasm
    _PUSH_ANY_simstack -> _simstack;
    true
enddefine;


;;; --- OPTIMISE TABLE FOR SYSTEM PROCEDURES -----------------------------

lconstant macro (
    OPTIM_TABLE_SIZE    = 2**4,
    OPTIM_HASH_MASK     = OPTIM_TABLE_SIZE-1,
    );

define lconstant macro OPTIM_HASH_TABLE assoc_list -> table;
    lvars idname, p, assoc_list, vec, table, cell, upd, pname;
    sysEXECUTE();
    {% repeat OPTIM_TABLE_SIZE times [] endrepeat %} -> table;
    until assoc_list == [] do
        dest(dest(assoc_list)) -> assoc_list -> vec -> idname;
        false -> upd;
        if islist(idname) then
            ;;; updater
            true -> upd, idname(2) -> idname
        endif;
        idname -> pname;
        if isstring(idname) then
            ;;; allow for section pathname
            procedure(proglist);
                dlocal proglist;
                sys_read_path(itemread(), false, false)
            endprocedure(pdtolist(incharitem(stringin(idname)))) -> idname;
            if ispair(word_dict_status(idname) ->> pname) then
                front(pname) -> pname
            else
                idname -> pname
            endif
        endif;
        sysPUSH(conspair(conspair(idname,consref("weakref")), "nonactive")),
        if upd then sysCALL("updater") endif;
        sysEXECUTE() -> p;
        ;;; hash on last char of procedure name
        (pname(datalength(pname)) && OPTIM_HASH_MASK) + 1 -> cell;
        p :: (vec :: table(cell)) -> table(cell)
    enduntil
enddefine;

define lconstant macro SR_ENTRIES list;
    lvars list, pdr_spec, sr_name, nres, nargs, pdr_name;
    for pdr_spec in list do
        explode(pdr_spec) -> (pdr_spec, nargs, nres, sr_name);
        if islist(pdr_spec ->> pdr_name) then pdr_name(2) -> pdr_name endif;
        nextif(lmember(pdr_name, [iscompound issimple isinteger]));
        sysPUSHQ(pdr_spec);     ;;; name or [-> name] for updater
        sysCALL( sysPUSH(consword('Opt_sr_' >< nargs >< 'a' >< nres >< 'r')),
                 sysPUSH(conspair(sr_name,consref({^pdr_name}))),
                 sysPUSHQ(2),
                    "consvector")
    endfor
enddefine;


    ;;; Note that OPTIM_HASH_TABLE weakrefs each procedure
lconstant
    opt_pdr_table = OPTIM_HASH_TABLE #_< [

    fast_front          {% Opt_fast_field,  _pint(@@P_FRONT) %}
    fast_back           {% Opt_fast_field,  _pint(@@P_BACK) %}
    fast_cont           {% Opt_fast_field,  _pint(@@RF_CONT) %}
    fast_idval          {% Opt_fast_field,  _pint(@@ID_VALOF) %}
    fast_destpair       {% Opt_fast_field,  false%}
    fast_subscrv         {% Opt_fast_subv,   _pint(@@V_WORDS) %}
    fast_prolog_functor {% Opt_fast_field,  _pint(@@PGT_FUNCTOR) %}
    fast_prolog_arg     {% Opt_fast_subv,   _pint(@@PGT_ARGS) %}
    fast_apply          {% Opt_fast_apply,  false %}
    chain               {% Opt_chain,       false %}
    fast_chain          {% Opt_chain,       true  %}
    prolog_assign       {% weakref[prolog_assign] Opt_plog_assign, false%}
    lisp_true           {% Opt_lisp_true,   false %}
    stacklength         {% Opt_stacklength, false %}
    '$-lisp$-Lisp_setstacklength' {% Opt_setstacklength, false %}
    dup                 {% Opt_dup,         false %}
    not                 {% Opt_not,         false %}
    dlocal_context      {% Opt_dlocal_context, "dlocal_context" %}
    dlocal_process      {% Opt_dlocal_context, "dlocal_process" %}
    +                   {% Opt_+-_1,        +_1 %}
    -                   {% Opt_+-_1,        -_1 %}
    ==                  {% Opt_cmp_op,      nonop _eq %}
    /==                 {% Opt_cmp_op,      nonop _neq %}
    fi_>                {% Opt_cmp_op,      nonop _sgr %}
    fi_>=               {% Opt_cmp_op,      nonop _sgreq %}
    fi_<                {% Opt_cmp_op,      nonop _slt %}
    fi_<=               {% Opt_cmp_op,      nonop _slteq %}
    fi_+                {% Opt_fi_+-,       true %}
    fi_-                {% Opt_fi_+-,       false %}
    iscompound          {% Opt_tag_test,    _iscompound %}
    issimple            {% Opt_tag_test,    _issimple %}
    isinteger           {% Opt_tag_test,    _isinteger %}

    ;;; procedures that take a procedure arg where we know that
    ;;; pushing a lex constant for the arg procedure doesn't necessitate
    ;;; 'run-time ident access' non-local lvars
    applist             {% Opt_try_unpush_lex, false %}
    maplist             {% Opt_try_unpush_lex, false %}
    appdata             {% Opt_try_unpush_lex, false %}
    appproperty         {% Opt_try_unpush_lex, false %}
    appproperty         {% Opt_try_unpush_lex, false %}
    fast_appproperty    {% Opt_try_unpush_lex, false %}

    ;;; updaters
    [-> fast_front]     {% Opt_ufast_field, _pint(@@P_FRONT) %}
    [-> fast_back]      {% Opt_ufast_field, _pint(@@P_BACK) %}
    [-> fast_cont]      {% Opt_ufast_field, _pint(@@RF_CONT) %}
    [-> fast_idval]     {% Opt_ufast_field, _pint(@@ID_VALOF) %}
    [-> fast_subscrv]           {% Opt_ufast_subv,   _pint(@@V_WORDS) %}
    [-> fast_prolog_functor]    {% Opt_ufast_field, _pint(@@PGT_FUNCTOR) %}
    [-> fast_prolog_arg]        {% Opt_ufast_subv,  _pint(@@PGT_ARGS) %}
    [-> fast_apply]     {% Opt_fast_apply,  true %}

    [-> applist]        {% Opt_try_unpush_lex, false %}
    [-> maplist]        {% Opt_try_unpush_lex, false %}

    % SR_ENTRIES #_<
        [
            [fsub_b         2 1 _subss]
            [fi_*           2 1 _pmult]
            [fi_//          2 2 _pdiv]
            [[-> fsub_b]    3 0 _u_subss]
        ]
#_IF DEF SUBROUTINE_OPTIMISE_LIST
            <> SUBROUTINE_OPTIMISE_LIST
#_ENDIF
        >_#
    %
    ] >_# ;



    ;;; plant code for call of system procedure
define Plant_syspdr_call(pdr);
    lvars list, props, pdr;
    if isword(pdr!PD_PROPS ->> props) then
        ;;; search for optimising procedure -- hash on last character of word
        props!W_STRING -> props;
        _subsv0(
            fast_subscrs(_pint(props!V_LENGTH), props) fi_&& OPTIM_HASH_MASK,
                                        opt_pdr_table) -> list;
        ;;; list is now an assoc list [<procedure> <vector> ... ]
        until list == [] do
            if fast_front(list) == pdr then
                fast_front(fast_back(list)) -> list;
                ;;; list is now {<optimising procedure> <arg>}
                ;;; if optimising procedure returns true then it has planted
                ;;; code, otherwise it has only set _simstack
                unless fast_apply(fast_subscrv(2,list), fast_subscrv(1,list))
                then
                    Plant(I_CALLABS, pdr, 2)
                endunless;
                return
            else
                fast_back(fast_back(list)) -> list
            endif
        enduntil
    endif;

    ;;; default is I_CALLABS and clear _simstack
    Plant(I_CALLABS, pdr, 2);
    _S_NONE -> _simstack
enddefine;

endsection;     /* $-Sys$-Vm */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 14 1997
        String16 changes
--- John Gibson, Jan 31 1997
        Made fsub_b be optimised to _subss instead of fast_subscrs.
--- John Gibson, Sep 27 1996
        Added Opt_tag_test for isinteger etc
--- John Gibson, May  4 1993
        Moved I_ procedures to vm_conspdr.p
--- John Gibson, Oct 16 1992
        Changed OPTIM_HASH_TABLE and SR_ENTRIES to use new weakref'ing
        mechanism built into sys_use_current_ident
--- John Gibson, Apr  2 1992
        Fixed bug in I_CMP (got optimised with a following I_IF_opt when that
        had an explicit operand)
--- John Gibson, Nov 28 1991
        Added fast_appproperty to opt_pdr_table
--- John Gibson, Sep 30 1991
        Fixed problem with I_CHAINPS planting an I_POP that gets optimised
        on the second pass. Also fixed -Replace_instr- to allow it to cope
        with the replacement code beginning with a label
--- John Gibson, Nov 28 1990
        Added I_MAKE_EXPTR
--- John Gibson, Dec 17 1989
        Changed I_LBLOCK to use new versions of run-time lex ident
        procedures.
--- John Gibson, Dec  8 1989
        Changes for new pop pointers
--- John Gibson, Jun 30 1989
        Added optimising for -fast_apply- and -fast_chain-
--- John Gibson, Jun  4 1989
        Replaced pop_optimise with not(pop_debugging)
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, Apr 30 1989
        Put into section $-Sys$-Vm.
--- John Gibson, Nov 23 1988
        Changes for lexical blocks.
--- John Gibson, Oct 21 1988
        Allowed for front of codelist label pairs to contain a
        redirecting label pair.
--- John Gibson, Aug 22 1988
        Lisp_setstacklength was not in section lisp -- corrected it.
--- John Gibson, Mar 11 1988
        Name changed to vm_optim.p (previously vmoptim.p).
        -Consprocedure- etc moved to vm_conspdr.p
--- John Gibson, Feb 26 1988
        Weakref'ed _plog_save_contn_sp
--- John Gibson, Feb 21 1988
        Moved -Opt_plog_assign- and -I_PLOG_RESTART- into vmplog.p
--- John Gibson, Feb 10 1988
        +_1 and -_1 now in section Sys
--- John Gibson, Dec 18 1987
        Added some missing declarations at top of file
--- John Gibson, Dec 18 1987
        Added declarations for _syschain, _sysncchain
--- John Gibson, Oct 31 1987
        Changes to -Consprocedure- to use csword-type mechanism
--- John Williams, Sep 29 1987
    Moved planting of integer check from I_GO_ON to sysGO_ON
 */
