/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/pop11_syntax.p
 > Purpose:         Defines main syntax words of Pop-11
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYNTAX, REF *POPSYNTAX
 */

;;; ---------------------- POP-11 SYNTAX ---------------------------------

#_INCLUDE '../lib/include/pop11_flags.ph'
#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'matchvar.ph'

constant
        procedure (sys_read_path, subscrl, sys_current_ident,
        sys_use_current_ident,
        pop11_FLUSHED, pop11_EMPTY, is_sub_syntax_word,
        pop11_exec_stmnt_seq_to, pop11_comp_stmnt_seq, pop11_comp_stmnt_seq_to,
        pop11_comp_expr_seq, pop11_comp_expr_seq_to,
        pop11_comp_expr_to, pop11_comp_prec_expr,
        pop11_need_nextitem, pop11_try_nextitem, pop11_try_nextreaditem,
        pop11_exec_compile
        )
    ;

vars
        procedure (sysGLOBAL, sysCOMPILE, sysCONSTRUCT,
        sysLBLOCK, sysENDLBLOCK, sysLVARS, sysDLVARS, sysVARS, sysLCONSTANT,
        sysCONSTANT, sysSYNTAX, sysLOCAL, sysDLABEL, sysDLABEL, sysIDENT,
        sysERASE, sysPASSIGN, sysUPASSIGN, sysSWAP,
        pop_expr_inst
        ),
        pop_syntax_only, pop_expr_item, pop_expr_update, pop_expr_prec,
        popclosebracket_exec, pop_new_lvar_list, pop_autoload,
        pop_pop11_flags, pop_vm_flags, pop_vm_compiling_list,
        pop_vm_dummy_idents, Sys$-Prglst$- _next_code
    ;

weak vars
        pop_#_include_stack
    ;

section $-Sys;

constant
        procedure (Conslist, Special_idprops,
        Pop11$-Comp_upd_expr_seq, Sect$-Curr_word_id,
        Vm$-Is_local_var, Vm$-Passign_ident, Vm$-SysPUSH_OR_IDENT
        )
    ;

vars
        Pop11$-loop_starts, Pop11$-loop_ends,
        Prglst$-was_nonmac, Prglst$- #_if_stack, Prglst$- _nextitem_code
    ;

endsection;


    ;;; Force inclusion of things referred to by name only in
    ;;; syntax code-planting.
uses-by_name (
        consclosure, dl, dup, explode, false, fi_-, isdefined,
        nil, pdprops, popstackmark, stacklength, sysanyvecons,
        sysconslist, sysconslist_onto, sysprarrow, pretty, sysvecons,
        true, updater, consmatchvar
    );

    ;;; Define prefix used for "define :<form>" syntax (Export this??)
lconstant define_prefix = 'define_';


;;; --- DECLARATIONS OF NON-OPENER SYNTAX WORDS -------------------------

constant
    Sys$-Pop11$-Syntax  = true; ;;; used to force inclusion of this file

constant syntax (
    ]               = pop_undef,
    }               = pop_undef,
    %               = pop_undef,
    |#              = pop_undef,
    define_define_form= pop_undef,
    define_form     = pop_undef,
    sub_syntax      = pop_undef,
    define_pdr_valof= pop_undef,
    pdr_valof       = pop_undef,
    enddefine       = pop_undef,
    endprocedure    = pop_undef,
    endlblock       = pop_undef,
    endgo_on        = pop_undef,
    macro           = pop_undef,
    syntax          = pop_undef,
    active          = pop_undef,
    updaterof       = pop_undef,
    with_nargs      = pop_undef,
    with_props      = pop_undef,
    \n              = pop_undef,
    ),

    syntax )        = pop_undef,            ;;; special for )
    syntax ,        = pop_undef,            ;;; special for ,
    syntax ;        = pop_undef,            ;;; special for ;
    ;

vars syntax (
    ^               = pop_undef,
    ^^              = pop_undef,
    );


;;; --- GENERAL SYNTAX CONSTRUCTS ----------------------------------------

constant syntax ( [, {, " );

lconstant macro (
    PL      = [$-Sys$-Prglst$-],    ;;; proglist section
    POP11   = [$-Sys$-Pop11$-],     ;;; POP11 section
    );

lconstant
    ica_errms           = '%ica: INCORRECT ITEM AFTER %S',
    ine_errms           = 'ine: IDENTIFIER NAME EXPECTED',
    inea_errms          = '%ine: IDENTIFIER NAME EXPECTED AFTER %S',

    ica_idstring        = 'pop11-ica:syntax',
    ine_idstring        = 'pop11-ine:syntax',
    invcontext_idstring = 'pop11-invcontext:syntax',
;

define lconstant Read_perm_path(item, construct) -> item;
    lvars id, item, construct;
    unless isword(sys_read_path(item, false, false) ->> item) then
        mishap(construct, item, 2, inea_errms, ine_idstring)
    endunless
enddefine;

define syntax testdef;
    lvars word = Read_perm_path(itemread(), "testdef");
    unless pop_syntax_only then
        if pop_pas_mode == "popc" then
            valof("popc_testdef_code")(word)
        else
            sysIDENT(conspair(word, "weakref"));
            sysCALL("isdefined");
        endif
    endunless;
    pop11_FLUSHED -> pop_expr_inst
enddefine;

define lconstant Read_ident_path(item);
    lvars item, depend_vec = "weakref";

    returnif(item /== "weakref") (dup(sys_read_path(item, false, false)));

    if islist(nextitem()) then
        [% "[", dl(itemread()), "]" %] nc_<> proglist -> proglist
    endif;
    if (itemread() ->> item) == "[" then
        ;;; read dependency list (as a vector)
        {%  until (itemread() ->> item) == "]" do
                nextif(item == ",");
                Read_perm_path(item, "weakref")
            enduntil
        %} -> depend_vec;
        itemread() -> item
    endif;

    dup(Read_perm_path(item, "weakref"));
    unless pop_syntax_only then conspair((), depend_vec) endunless
enddefine;

define syntax weakref;
    Read_ident_path("weakref") -> (, pop_expr_item);
    sysPUSH -> pop_expr_inst
enddefine;

define lconstant Tryupd_xsqcomp(updating);
    lvars updating;
    if updating then
        ;;; allow only expression seq if update mode -- expressions
        ;;; are evaluated backwards in update mode
        POP11 Comp_upd_expr_seq(updating)
    else
        ;;; allow statement seq
        pop11_comp_stmnt_seq()
    endif
enddefine;

define lconstant Delayed_compile(compile_pdr, upd, proglist);
    lvars upd, procedure compile_pdr;
    dlocal proglist;
    compile_pdr(false)
enddefine;
;;;
define updaterof Delayed_compile(compile_pdr, upd, proglist);
    lvars upd, procedure compile_pdr;
    dlocal proglist;
    compile_pdr(upd)
enddefine;

define syntax -1 (;
    lvars procedure inst = pop_expr_inst, nxtitem;
    dlocal popclosebracket = ")", pop_syntax_only;

    define lconstant Tryupd_txsqcomp(popclosebracket, updating);
        lvars updating;
        dlocal popclosebracket;
        Tryupd_xsqcomp(updating);
        pop11_need_nextitem(popclosebracket) ->
    enddefine;

    if inst == pop11_EMPTY then
        ;;; not in operator position - just compile bracketed expr
        pop11_FLUSHED -> pop_expr_inst;
        if pop_expr_update then
            ;;; delay the interpretation -- just read thru the expr seq
            true -> pop_syntax_only;
            Delayed_compile(%Tryupd_txsqcomp, pop_expr_update, proglist%)
                                                            -> pop_expr_inst;
            popclosebracket -> pop_expr_item
        endif;
        pop11_comp_stmnt_seq()
    elseif (nextitem() ->> nxtitem) == "%" then
        ;;; partial application
        PL Chop();              ;;; remove the %
        pop_expr_inst(pop_expr_item);       ;;; flush pdpart expression
        pop11_FLUSHED -> pop_expr_inst;
        procedure;
            dlocal pop_new_lvar_list;
            lvars len_var = sysNEW_LVAR();
            sysCALL("stacklength"), sysPOP(len_var);
            pop11_comp_expr_seq_to("%") -> ;    ;;; compile frozvals
            sysCALL(sysPUSH(len_var), sysPUSH("consclosure"), "sysanyvecons");
        endprocedure();
        nextitem() ->                   ;;; so _nextitem_code is set
    elseif inst == sysPUSH then
        ;;; call of identifier
        pop11_comp_expr_seq();
        sysCALL -> pop_expr_inst
    elseif inst == sysPUSHQ then
        ;;; call of quoted structure
        pop11_comp_expr_seq();
        sysCALLQ -> pop_expr_inst
    else
        ;;; call of whatever's on the stack
        inst(pop_expr_item);            ;;; flush it
        if nxtitem == popclosebracket then
            ;;; no args to call -- procedure is tos
            sysCALLS
        else
            procedure;
                dlocal pop_new_lvar_list;
                sysPOP(sysNEW_LVAR() ->> pop_expr_item);
                pop11_comp_expr_seq()
            endprocedure();
            sysCALL
        endif -> pop_expr_inst
    endif;
    ;;; (fast) check for closing bracket
    if PL _nextitem_code _bitst _:M_ID_IS_WORD
    and fast_front(proglist) == ")" then
        PL Chop()
    else
        pop11_need_nextitem(")") ->
    endif;
enddefine;

define lconstant Label_syntax(labelling_pdr);
    lvars procedure labelling_pdr;
    unless pop_expr_inst == sysPUSH and isword(pop_expr_item) then
        mishap(0, 'INCORRECT LABEL SYNTAX', 'pop11-ils:syntax')
    else
        labelling_pdr(pop_expr_item);
        ;;; restart the current expression
        chainfrom(pop_expr_prec, pop_expr_update, pop11_comp_prec_expr,
                                                pop11_comp_prec_expr)
    endunless
enddefine;

define syntax -1 : ;
    Label_syntax(sysLABEL)
enddefine;

define syntax -1 :* ;
    Label_syntax(sysDLABEL)
enddefine;

define syntax 1 .;
    pop_expr_inst(pop_expr_item); pop11_FLUSHED -> pop_expr_inst;
    pop11_comp_prec_expr(INTERNAL_OP_PREC 1, false) -> ;
    sysCALLS -> pop_expr_inst
enddefine;

define syntax 9 and;
    lvars label;
    pop_expr_inst(pop_expr_item); pop11_FLUSHED -> pop_expr_inst;
    sysAND(sysNEW_LABEL() ->> label);
    pop11_comp_prec_expr(INTERNAL_OP_PREC 9, false) -> ;
    sysLABEL(label)
enddefine;

define syntax 10 or;
    lvars label;
    pop_expr_inst(pop_expr_item); pop11_FLUSHED -> pop_expr_inst;
    sysOR(sysNEW_LABEL() ->> label);
    pop11_comp_prec_expr(INTERNAL_OP_PREC 10, false) -> ;
    sysLABEL(label)
enddefine;

    ;;; procedure passed to pop11_comp_prec_expr for pop_expr_update
    ;;; by -> and ->> is applied for an empty update expression
define lconstant upd_erase();
    sysERASE(0)     ;;; dummy arg
enddefine;

define syntax 11 -> ;
    pop_expr_inst(pop_expr_item); pop11_FLUSHED -> pop_expr_inst;
    pop11_comp_prec_expr(INTERNAL_OP_PREC 11, upd_erase) ->
enddefine;

define syntax 11 ->> ;
    pop_expr_inst(pop_expr_item); pop11_FLUSHED -> pop_expr_inst;
    sysPUSHS(false);            ;;; takes dummy argument
    pop11_comp_prec_expr(INTERNAL_OP_PREC 11, upd_erase) ->
enddefine;


define syntax 12 =>;
    pop_expr_inst(pop_expr_item); pop11_FLUSHED -> pop_expr_inst;
    sysPUSH(if popclosebracket == popclosebracket_exec then
                "true"
            else
                "false"
            endif);
    sysCALL("sysprarrow");
    ";" :: proglist -> proglist
enddefine;

define syntax 12 ==>;
    pop_expr_inst(pop_expr_item); pop11_FLUSHED -> pop_expr_inst;
    sysCALL("pretty");
    ";" :: proglist -> proglist
enddefine;

define syntax #| ;
    dlocal pop_new_lvar_list;
    lvars len_var = sysNEW_LVAR();
    sysCALL("stacklength"), sysPOP(len_var);
    pop11_comp_stmnt_seq_to("|#") -> ;
    sysCALL(sysCALL("stacklength"), sysPUSH(len_var), "fi_-")
enddefine;

lconstant go_on_marker = 'go_on_marker';

define lconstant Check_label(item, new_go_on) -> item;
    lvars item, new_go_on, id;
    if not(isword(item)) or ((sys_current_ident(item) ->> id)
    and id!ID_IDENTPROPS _bitst (_:M_ID_SYNTAX _biset _:M_ID_OPERATOR))
    then
        mishap(item, 1, 'goto OR go_on: INVALID LABEL',
                                            'pop11-invlab:syntax')
    elseif not(new_go_on) and id
    and isclosure(fast_idval(id) ->> id) and id!PD_PROPS == go_on_marker
    then
        id(itemread(), go_on_marker) -> item
    endif
enddefine;

define syntax goto;
    sysGOTO(Check_label(readitem(), false))
enddefine;

define lconstant New_go_on();
    lvars   i, prefix, lprefix, clos, endlab, int_bits = 0, min_int = false,
            max_int, def_lab = false;

    define lconstant gen_label(arg);
        lvars arg, is_goto = false, normalise;
        if arg == go_on_marker then () -> arg, true -> is_goto endif;
        if isinteger(arg) then
            unless is_goto then
                0 -> normalise;
                unless min_int then
                    arg ->> min_int -> max_int
                elseif arg fi_< min_int then
                    min_int - arg -> normalise;
                    arg -> min_int
                elseif arg fi_> max_int then
                    arg -> max_int
                endunless;
                if max_int - min_int > 2:1e10 then
                    mishap(0, 'INTEGER SPREAD TOO LARGE IN go_on')
                endif;
                (int_bits << normalise) || (1 << (arg-min_int)) -> int_bits
            endunless
        elseunless arg == "default" or arg == "end" then
            mishap(arg, 1, 'ILLEGAL ARGUMENT FOR go_on LABEL',
                                            'pop11-invlab:syntax')
        endif;
        consword(lprefix sys_>< arg);
        if not(is_goto) and arg == "default" then dup() -> def_lab endif
    enddefine;

    Check_label(readitem() ->> prefix, true) -> ;
    readitem() -> ;         ;;; skip :

    ;;; setup prefix
    sysLCONSTANT(prefix, "macro");
    gen_label -> clos;
    go_on_marker -> clos!PD_PROPS;
    sysPASSIGN(clos, prefix);
    'go_on-' sys_>< prefix sys_>< '-' -> lprefix;

    pop11_comp_stmnt_seq_to("endgo_on") -> ;
    consword(lprefix sys_>< "end") -> endlab;
    unless def_lab then
        sysGOTO(endlab);
        sysLABEL(clos("default") ->> def_lab);
        sysPUSHQ(fast_word_string(prefix));
        sysPUSHQ(1);
        sysPUSHQ('%NO LABEL FOR VALUE IN go_on ... to %P:');
        sysCALL("mishap")
    endunless;
    sysLABEL(endlab);

    min_int -> i;
    [%  until int_bits == 0 do
            if int_bits &&/=_0 1 then
                consword(lprefix sys_>< i)
            else
                def_lab
            endif;
            int_bits >> 1 -> int_bits;
            i+1 -> i
        enduntil
    %], def_lab, min_int or 1       ;;; return 3 args for sysGO_ON
enddefine;

define syntax go_on;
    lvars item, got_else, lablist;
    lconstant syntax_idstring = 'pop11-go_on:syntax';

    procedure;
        dlocal proglist, pop_syntax_only = true;
        pop11_comp_expr_to("to") -> ;
        readitem() -> ;
        nextreaditem()
    endprocedure() -> item;

    if item == ":" then
        ;;; new style
        sysLBLOCK(false);
        pop11_comp_expr_to("to") -> ;
        sysGO_ON(New_go_on);
        sysENDLBLOCK();
        return
    endif;

    ;;; old style
    false -> got_else;
    pop11_comp_expr_to("to") -> ;
    [% until (readitem() ->> item) == ";" do
        unless item == "," then
            unless item == "else" then
                Check_label(item, false) -> item
            endunless;
            if got_else==true then item -> got_else
            elseif got_else then
                mishap(item, 1, 'UNEXPECTED ITEM IN go_on STATEMENT');
            elseif item == "else" then true -> got_else
            else item
            endif;
        endunless;
    enduntil %] -> lablist;
    if got_else==true then
        mishap(0, 'MISSING LABEL AFTER else IN go_on STATEMENT')
    endif;
    sysGO_ON(lablist, got_else);
    item :: proglist -> proglist
enddefine;

define syntax return;
    if pop11_try_nextitem("(") then
        pop11_comp_expr_seq_to(")") ->
    endif;
    sysGOTO("return")
enddefine;

define lconstant Compile_return(ifso);
    lvars ifso, lab;
    pop11_need_nextitem("(") ->;
    pop11_comp_expr_to(")") ->;
    if pop11_try_nextitem("(") then
        sysNEW_LABEL() -> lab;
        if ifso then sysIFNOT else sysIFSO endif(lab);
        pop11_comp_expr_seq_to(")") ->;
        sysGOTO("return");
        sysLABEL(lab)
    else
        if ifso then sysIFSO else sysIFNOT endif("return")
    endif
enddefine;

define syntax returnif;
    Compile_return(true)
enddefine;

define syntax returnunless;
    Compile_return(false)
enddefine;

define lconstant Escape_word_check(_bit);
    lvars item, id, _bit;
    if isword(Read_ident_path(itemread()) -> pop_expr_item ->> item)
    and pop_syntax_only
        or ((sys_current_ident(item) ->> id) and id!ID_IDENTPROPS _bitst _bit)
    then
        sysPUSH -> pop_expr_inst
    else
        mishap(pdprops(caller(1)), item, 2, ica_errms, ica_idstring)
    endif
enddefine;

define syntax nonop;
    Escape_word_check(_:M_ID_OPERATOR)
enddefine;

define syntax nonsyntax;
    Escape_word_check(_:M_ID_SYNTAX)
enddefine;

lconstant procedure Nonactive = conspair(%"nonactive"%);

define syntax nonactive;
    Escape_word_check(_:M_ID_ACTIVE);
    Nonactive(pop_expr_item) -> pop_expr_item
enddefine;

define syntax ident;
    lvars item;
    if isword(Read_ident_path(itemread()) -> pop_expr_item ->> item) then
        sysIDENT -> pop_expr_inst
    else
        mishap("ident", item, 2, inea_errms, ine_idstring)
    endif
enddefine;

define syntax ";
    lvars item = readitem(), word, id, keyword;
    if isstring(item) then
        consword(item) -> item
    elseif (item == "ident" or item == "nonactive") and nextitem() /== """ then
        ;;; word identifier
        nonsyntax ident();
        item -> keyword;
        pop_expr_item -> item;      ;;; arg for sysIDENT
        unless pop_syntax_only then
            if ispair(item) then fast_front(item) else item endif -> word;
            if isword(word!W_DICT_NEXT) then
                ;;; not already a word identifier
                sys_use_current_ident(item) -> (id, );
                if id!ID_IDENTPROPS _bitst _:M_ID_LEX then
                    mishap(word, 1, 'CAN\'T GET WORD-IDENTIFIER FOR LEXICAL IDENTIFIER',
                                        'pop11-iqw:name-ref-inval')
                endif;
                Sys$-Sect$-Curr_word_id(word)
                    -> if ispair(item) then fast_front(item) else item endif
            endif
        endunless;
        if keyword == "nonactive" then Nonactive(item) -> item endif
    elseunless isword(item) then
        mishap(item, 1, 'iqw: INCORRECT QUOTED WORD', 'pop11-iqw:syntax')
    endif;

    pop11_need_nextitem(""") -> ;
    item -> pop_expr_item;
    sysPUSHQ -> pop_expr_inst
enddefine;


;;; --- CONDITIONALS IF AND UNLESS -------------------------------------------

define lconstant Do_condition_compile(closebracket, syspdr);
    lvars closebracket, procedure syspdr;
    dlocal pop_syntax_only;

    define lconstant Cond_compile(popclosebracket, updating, syspdr);
        lvars item, lab, procedure syspdr;
        dlvars endlab, updating;
        dlocal popclosebracket, POP11 loop_starts, POP11 loop_ends;

        define lconstant Cond_clause(lab, syspdr) -> lab;
            lvars lab, procedure syspdr;
            if lab then
                sysGOTO(endlab);
                sysLABEL(lab)
            endif;
            pop11_comp_expr_to([then do]) -> ;
            sysNEW_LABEL() -> lab;
            syspdr(lab);
            Tryupd_xsqcomp(updating)
        enddefine;

        sysNEW_LABEL() -> endlab;
        Cond_clause(false, syspdr) -> lab;
        while (pop11_try_nextitem([elseif elseunless]) ->> item) do
            Cond_clause(lab,if item == "elseif" then sysIFNOT
                            else sysIFSO
                            endif) -> lab
        endwhile;
        if pop11_try_nextitem("else") then
            sysGOTO(endlab);
            sysLABEL(lab);
            Tryupd_xsqcomp(updating)
        else
            sysLABEL(lab)
        endif;
        pop11_need_nextitem(popclosebracket) -> ;
        sysLABEL(endlab)
    enddefine;

    if pop_expr_update then
        ;;; special for "if" and "unless",
        ;;; allowing these constructs in update mode
        true -> pop_syntax_only;
        Delayed_compile(%Cond_compile(%syspdr%), pop_expr_update, proglist%)
                                                            -> pop_expr_inst;
        closebracket -> pop_expr_item
    endif;
    Cond_compile(closebracket, false, syspdr)
enddefine;

define syntax if;
    Do_condition_compile([endif {close}], sysIFNOT)
enddefine;

define syntax unless;
    Do_condition_compile([endunless {close}], sysIFSO)
enddefine;


;;; --- MATCHVARS ---------------------------------------------------------

    ;;; sysCONSTRUCT types
lconstant macro (
    SCON_LIST   = 1,
    SCON_VECONS = 2,
    SCON_VECTOR = 3,
    SCON_RECORD = 4
);

define lconstant Compile_=?(has_id, flags);
    lvars has_id, flags, idname, props, item, _idprops;
    if has_id then
        if pop11_try_nextreaditem(""") then
            nonsyntax " ();
            sysPUSHQ(pop_expr_item ->> item);
            sysPUSHQ(item)
        else
            nonsyntax ident();
            pop_expr_item ->> idname -> props;
            unless pop_syntax_only then
                if ispair(idname!W_DICT_NEXT) then
                    ;;; word identifier -- use the base word
                    fast_front(idname!W_DICT_NEXT) -> props
                endif
            endunless;
            pop11_define_props(idname, props, false) -> props;
            sysPUSHQ(props);
            sysIDENT(idname)
        endif
    else
        sysPUSHQ(false);
        sysPUSHQ(false)
    endif;
    if pop11_try_nextreaditem([: #]) ->> item then
        if item == "#" then flags || M_MV_CONV_P -> flags endif;
        unless isword(nextitem()) then
            Sys$-Check_integer(readitem() ->> item, 0);
            sysPUSHQ(item)
        elseif pop11_try_nextitem("%") then
            pop11_comp_expr_to("%") ->
        else
            nonsyntax ident();
            Sys$-Vm$-SysPUSH_OR_IDENT(pop_expr_item)
        endunless
    else
        sysPUSHQ(false)
    endif;
    sysPUSHQ(flags);
    pop11_FLUSHED -> pop_expr_inst;

    4, "consmatchvar"
enddefine;

define syntax =? ;
    sysCONSTRUCT(true,              0, Compile_=?, SCON_RECORD)
enddefine;

define syntax =?? ;
    sysCONSTRUCT(true, M_MV_SEQ_MATCH, Compile_=?, SCON_RECORD)
enddefine;

define syntax =* ;
    sysCONSTRUCT(false,             0, Compile_=?, SCON_RECORD)
enddefine;

define syntax =** ;
    sysCONSTRUCT(false, M_MV_SEQ_MATCH, Compile_=?, SCON_RECORD)
enddefine;


;;; --- LIST AND VECTOR CONSTRUCTORS -------------------------------------

vars
    Sys$-Pop11$-cons_constant = false;

lvars
    struct_trailing_^^  = false;

lconstant constructor_sub_syntax = [%
        "[",    nonsyntax [,
        "{",    nonsyntax {,
        "=?",   nonsyntax =?,
        "=??",  nonsyntax =??,
        "=*",   nonsyntax =*,
        "=**",  nonsyntax =**
    %];

define pop11_comp_constructor(popclosebracket);
    lvars item, item2, p, save_constant = POP11 cons_constant, count = 0;
    dlocal popclosebracket;

    lconstant not_after_^ = [% "[", "]", "{", "}", "%", "^", "^^" %],
              errms = 'ica: INCORRECT ITEM AFTER ^ OR ^^' ;

    pop_pop11_flags &&/=_0 POP11_CONSTRUCTOR_CONSTS -> POP11 cons_constant;

    until (readitem() ->> item) == popclosebracket do
        if item == termin or item == "]" or item == "}" then
            mishap(item, consword('EXPECTING'), popclosebracket, 3,
                                        'mcb: MISSING CLOSING BRACKET',
                                        'pop11-mcb:syntax')
        endif;
        if item == "%" then
            pop11_comp_stmnt_seq_to("%") -> ;
            false -> POP11 cons_constant        ;;; not constant
        elseif item == "^" or item == "^^" then
            lconstant mspart = '^ OR ^^';
            if isword(nextreaditem()) then
                itemread() -> item2;
                if item2 == "(" then
                    pop11_comp_stmnt_seq_to(")") ->
                elseunless lmember(item2, not_after_^) then
                    if isword(item2) then
                        sysPUSH(sys_read_path(item2, false, false))
                    else
                        sysPUSHQ(item2)
                    endif
                else
                    mishap(mspart, item2, 2, ica_errms, ica_idstring)
                endif
            else
                mishap(mspart, nextreaditem(), 2, ica_errms, ica_idstring)
            endif;
            false -> POP11 cons_constant;      ;;; not constant

            if item == "^^" then
                readitem() -> item;
                if popclosebracket == "]" and item == "]" then
                    true -> struct_trailing_^^;
                    return(false)
                else
                    item :: proglist -> proglist;
                    sysCALL(if popclosebracket == "]" then "dl" else "explode" endif);
                endif
            endif
        elseif Sys$-list_assoc_val(item, constructor_sub_syntax) ->> p then
            fast_apply(p)
        else
            sysPUSHQ(item)
        endif;

        count fi_+ 1 -> count
    enduntil;

    false -> struct_trailing_^^;
    if POP11 cons_constant then
        count;
        save_constant -> POP11 cons_constant
    else
        false
    endif
enddefine;

define syntax [;
    lvars savepl = proglist;
    dlocal popclosebracket = "]";
    if readitem() == "]" then sysPUSH("nil"); return endif;
    savepl -> proglist;
    sysCONSTRUCT(
        procedure();
            pop11_comp_constructor("]"),
            if struct_trailing_^^ then
                ;;; last item in list was ^^var
                "sysconslist_onto"
            else
                "sysconslist"
            endif
        endprocedure, SCON_LIST)
enddefine;

define syntax {;
    sysCONSTRUCT(
        procedure();
            pop11_comp_constructor("}"), "sysvecons"
        endprocedure, SCON_VECONS)
enddefine;

define syntax cons_with;
    sysCONSTRUCT(
        procedure();
            lvars count;
            pop11_comp_expr_to("{") -> ;    ;;; leave constructor on stack
            (pop11_comp_constructor("}") ->> count) and count fi_+ 1,
                                        "sysanyvecons"
        endprocedure, SCON_VECONS)
enddefine;


;;; --- LBLOCK ------------------------------------------------------------

define syntax lblock;
    lvars executing = popclosebracket == popclosebracket_exec;
    dlocal pop_pop11_flags;

    sysLBLOCK(executing);
    if executing then
        pop11_exec_stmnt_seq_to
    else
        pop11_comp_stmnt_seq_to
    endif("endlblock") -> ;
    sysENDLBLOCK()
enddefine;


;;; --- PROCEDURE AND DEFINE ----------------------------------------------

lconstant
    def_error_ms = conspair('ips: INCORRECT PROCEDURE SYNTAX',
                                'pop11-ips:syntax');

lvars
    pdr_error_ms        = def_error_ms,
    local_init_list     = false,
    local_dummy_idents  = [],
    ;

define lconstant Pdr_error(mess);
    lvars mess;
    chain(fast_front(pdr_error_ms) <> ' (' <> mess <> ')',
                                        fast_back(pdr_error_ms), mishap)
enddefine;


    /*  Called only when pop_vm_compiling_list /== []
        Redefined by POPC.
    */
define vars pop11_vars_default_check(idname);
    lvars idname;
    printf(';;; WARNING: %p DEFAULTED TO VARS IN PROCEDURE %p\n',
                            [%idname, pdprops(hd(pop_vm_compiling_list))%]);
    sysVARS
enddefine;

define vars pop11_define_declare(p_idname, globl_p, decl_p, idprops);
    lvars globl_p, decl_p, idprops, p_idname;
    unless decl_p then
        sysVARS -> decl_p;
        if pop_vm_compiling_list == [] then
            ;;; top level define
            if pop_pop11_flags &&/=_0 POP11_DEFINE_CONSTANT then
                sysCONSTANT -> decl_p
            endif
        else
            ;;; nested define
            if pop_pop11_flags &&/=_0 POP11_OLD_VARS then
                if pop_pop11_flags &&/=_0 POP11_VARS_CHECK then
                    pop11_vars_default_check(p_idname) -> decl_p
                endif
            else
                ;;; new style -- defaults to lconstant
                sysLCONSTANT -> decl_p
            endif
        endif
    endunless;
    unless idprops then
        if pop_pop11_flags &&/=_0 POP11_DEFINE_PROCEDURE then
            "procedure"
        else
            0
        endif -> idprops
    endunless;
    if decl_p == sysVARS or decl_p == sysCONSTANT then
        unless globl_p or pop_pop11_flags &&=_0 POP11_PERM_GLOBAL then
            ;;; default global if POP11_PERM_GLOBAL
            sysGLOBAL -> globl_p
        endunless
    elseif globl_p then
        Pdr_error(0, 'not vars or constant after global/nonglobal')
    endif;
    decl_p(p_idname, idprops);
    if globl_p then globl_p(p_idname) endif
enddefine;

define vars pop11_define_props(p_idname, props, upd) -> props;
    lvars p_idname, props, upd;
    if not(pop_debugging)
    and pop_pop11_flags &&/=_0 POP11_NO_LEX_PDPROPS
    and sys_current_ident(p_idname)!ID_IDENTPROPS _bitst _:M_ID_LEX then
        ;;; false for lex props
        false -> props
    ;;; else default is to leave props alone
    endif
enddefine;


define lconstant Read_idprops(item, allow_act) -> (idprops, item);
    lvars item, idprops = false, actmult = false, allow_act;
    if allow_act and item == "active" then
        if (readitem() ->> item) == ":" then
            ;;; multiplicity
            if isinteger(readitem() ->> actmult) then
                readitem() -> item
            else
                mishap('active:', actmult, 2, ica_errms, ica_idstring)
            endif
        else
            ;;; default multiplicity is 1
            1 -> actmult
        endif
    endif;
    if item == "procedure" or item == "macro" or item == "syntax"
    or isnumber(item) then
        item -> idprops;
        readitem() -> item;
        ;;; see if syntax operator
        if idprops == "syntax" and isnumber(item) then
            Sys$-Special_idprops("syntax", item) -> idprops;
            readitem() -> item
        endif
    endif;
    if actmult then
        conspair(if idprops then idprops else 0 endif, actmult) -> idprops
    endif
enddefine;

define lconstant Make_dlocal_expr(item, attribute) -> (item, name);
    lvars item, attribute, name;

    define lconstant Get_dlexpr_pdr() -> p;
        lvars procedure p;
        dlocal pop_syntax_only = true;

        define lconstant exprcomp(upd);
            lvars upd;
            pop11_comp_prec_expr(INTERNAL_OP_PREC 12.8, upd) ->
        enddefine;

        Delayed_compile(%exprcomp, true, proglist%) -> p;
        if pop11_comp_expr_to(#_< [, %"%"%] >_#) == "," then
            ;;; explicit put expression
            Delayed_compile(%exprcomp, true, proglist%) -> updater(p);
            pop11_comp_expr_to("%") ->
        endif
    enddefine;

    if item == "%" then
        ;;; dlocal expression
        if attribute == "nonactive" then
            mishap("nonactive", 1, inea_errms, ine_idstring)
        elseunless attribute then
            ;;; default multiplicity is 1
            1 -> attribute
        endif;
        Get_dlexpr_pdr() -> item;
        attribute -> pdprops(item);     ;;; multiplicity
        false -> name
    else
        if isinteger(attribute) then
            mishap(item, 1, 'dlocal: EXPRESSION EXPECTED AFTER MULTIPLICITY',
                                ica_idstring)
        endif;
        unless isword(Read_ident_path(item) -> item ->> name) then
            mishap(name, 1, ine_errms, ine_idstring)
        endunless;
        if attribute then Nonactive(item) -> item endif
    endif;
    sysLOCAL(item)
enddefine;

define lconstant Get_pdargs(add_dummies) -> arglist;
    lvars item, arglist = [], savepl, idprops, add_dummies, brack, c, _id;
    pop11_try_nextreaditem("(") -> brack;
    repeat
        proglist -> savepl;
        Read_idprops(readitem(), false) -> (idprops, item);
        if not(idprops) and isword(item)
        and (item == "="
            or (sys_current_ident(item) ->> _id)
                and _id!ID_IDENTPROPS _bitst _:M_ID_SYNTAX
                and _id!ID_IDENTPROPS _bitst _:M_ID_CONSTANT
                and item /== "$-")
        then
            quitunless(item == ",")
        else
            unless isword(sys_read_path(item, false, false) ->> item) then
                Pdr_error(item, 1, 'word needed')
            endunless;
            if add_dummies then
                conspair(item, local_dummy_idents) -> local_dummy_idents
            endif;
            if idprops then
                conspair(idprops, item) -> item
            endif;
            conspair(item, arglist) -> arglist
        endif
    endrepeat;
    savepl -> proglist;
    pop11_try_nextreaditem(")") -> c;
    if brack then
        unless c then
            Pdr_error(0, 'missing closing ")" in header')
        endunless
    else
        if c then
            Pdr_error(0, 'missing opening "(" in header')
        endif
    endif
enddefine;

define lconstant Is_decl_keyword(item, want_decl_p);
    lvars item, decl_p_id, want_decl_p;
    unless isword(item) and iscompound(item!W_IDENTIFIER ->> item)
    and (Sys$-list_assoc_val(item,
                    #_< [%  ident lvars,    ident sysLVARS,
                            ident dlvars,   ident sysDLVARS,
                            ident lconstant,ident sysLCONSTANT,
                            ident vars,     ident sysVARS,
                            ident constant, ident sysCONSTANT,
                            ident dlocal,   ident sysLOCAL
                        %] >_# ) ->> decl_p_id)
    then
        false
    elseif want_decl_p then
        fast_idval(decl_p_id)
    else
        fast_idval(item)
    endunless
enddefine;

define lconstant Plant_assign(token, lconst);
    lvars t, token, lconst;
    lconstant workp = writeable conspair(0,[]);
    unless islist(token) then
        token -> fast_front(workp);
        workp -> token
    endunless;
    fast_for t in token do
        if t then
            if lconst then
                sysPASSIGN((), t)
            else
                () -> if isprocedure(t) then t()    ;;; dlocal expr
                      else sysPUSH(t)               ;;; ident name/pair
                      endif
            endif
        else
            if lconst then
                ->
            else
                sysERASE(0)
            endif
        endif
    endfor
enddefine;


    /*  See if any alternate value for nargs or props
    */
define lconstant Try_with(props, nargs) -> (props, nargs);
    lvars item, savewhich, props, nargs;
    while pop11_try_nextreaditem([with_nargs with_props]) ->> savewhich do
        procedure;
            dlocal pop_autoload = false;
            itemread()
        endprocedure() -> item;
        if savewhich == "with_nargs" then
            if isinteger(item) and 0 fi_<= item and item fi_< 255 then
                item -> nargs
            else
                Pdr_error(item, 1, 'illegal value for with_nargs')
            endif
        else
            if item == "false" then false else item endif -> props
        endif
    endwhile
enddefine;

define pop11_comp_procedure(popclosebracket, p_idname, props) -> pdr;
    lvars   item, nargs, pdr, outputs, inputs, declare_var,
            init_list, props, p_idname;
    dlocal  popclosebracket, POP11 loop_starts, POP11 loop_ends,
            local_init_list = false, pop_pop11_flags,
            local_dummy_idents = pop_vm_dummy_idents;

    define lconstant Decl_pdargs(arglist);
        lvars arglist, arg, idprops;
        fast_for arg in arglist do
            0 -> idprops;
            if ispair(arg) then fast_destpair(arg) -> (idprops, arg) endif;
            nextif(Sys$-Vm$-Is_local_var(arg) or arg == "_");
            ;;; not already an lvar or dlocal, etc
            if pop_pop11_flags &&/=_0 POP11_OLD_VARS then
                if pop_pop11_flags &&/=_0 POP11_VARS_CHECK then
                    pop11_vars_default_check(arg)
                else
                    ;;; defaults to vars
                    sysVARS
                endif
            else
                ;;; new style -- defaults to lvars
                sysLVARS
            endif (arg, idprops)
        endfor
    enddefine;

    ;;; get inputs
    Get_pdargs(true) -> inputs;
    listlength(inputs) -> nargs;            ;;; no of formal args

    ;;; get outputs
    [] -> outputs;
    while pop11_try_nextreaditem([-> =>]) do
        rev(Get_pdargs(true)) -> item;
        if item == [] then
            Pdr_error(0, 'nothing after -> or =>');
        endif;
        item <> outputs -> outputs
    endwhile;

    ;;; see if any alternate value for nargs or props
    Try_with(props, nargs) -> (props, nargs);

    ;;; check end of procedure heading
    unless pop11_try_nextreaditem(";") then
        Pdr_error(0, 'missing ; after header')
    endunless;

    ;;; compile procedure body
    sysPROCEDURE(p_idname, nargs);          ;;; commence code for new proc

    ;;; See if any declaration statements immediately follow the procedure
    ;;; heading and compile these before declaring the argument and result
    ;;; variables. Because the code to pop the args has not yet been
    ;;; generated, any initialisations contained in these statements must be
    ;;; saved up and done after the args are popped. This is achieved by
    ;;; setting local_init_list to a list, which causes
    ;;; pop11_comp_declaration to save the initialisation code and the var
    ;;; name in this list.
    [] -> init_list;
    repeat
        procedure;
            ;;; treat procedure formals as dummy idents because they're
            ;;; not yet declared
            dlocal pop_vm_dummy_idents = local_dummy_idents;
            nextitem()
        endprocedure() -> item;
        if item == ";" then
            PL Chop();
            nextloop
        elseif Is_decl_keyword(item, false) ->> item then
            ;;; returned syntax procedure for keyword
            PL Chop();
            ;;; must only have local_init_list true while executing the
            ;;; declaration procedure
            init_list -> local_init_list;
            fast_apply(item);       ;;; apply syntax procedure
            local_init_list -> init_list;
            false -> local_init_list
        else
            quitloop
        endif
    endrepeat;
    Decl_pdargs(inputs);            ;;; declare inputs
    Decl_pdargs(outputs);           ;;; declare outputs

    ;;; generate pops for inputs
    for item in inputs do
        sysPOP(if ispair(item) then fast_back(item) else item endif)
    endfor;

    ;;; now process any saved-up initialisations
    rev(init_list) -> inputs;           ;;; rev to keep correct order
    applist(inputs, procedure(token);
                        lvars token;
                        dlocal proglist, PL was_nonmac;
                        destpair(token) -> (token, proglist);
                        ;;; recompile init expression
                        pop11_comp_expr_to([, ) ;]) -> ;
                        Plant_assign((), token, false)
                    endprocedure);

    [] ->> POP11 loop_starts -> POP11 loop_ends;
    pop11_comp_stmnt_seq_to(popclosebracket) ->;    ;;; procedure body
    sysLABEL("return");

    ;;; generate pushes for outputs
    for item in outputs do
        sysPUSH(if ispair(item) then fast_back(item) else item endif)
    endfor;

    sysENDPROCEDURE() -> pdr;
    unless pop_syntax_only then props -> pdr!PD_PROPS endunless
enddefine;      /* pop11_comp_procedure */

define syntax define;
    lvars   item, num_opargs, p_idname, upd, assign_name, globl, decl_pdr,
            idprops, props, list, subsyntax = false, pdr_val = false,
            savepl, nargs, id, _nonactive;
    dlocal  pop_syntax_only,
            pdr_error_ms = #_< conspair('ids: INCORRECT DEFINE SYNTAX',
                                        'pop11-ids:syntax') >_#;
    lconstant df_err_ms = 'invalid declarations for define_form';

    define lconstant Restarg() with_nargs 2;
        lvars arg = subscrl();
        if isvector(arg) then
            explode(arg)
        elseif ispair(arg) then
            fast_destpair(arg)
        else
            arg
        endif
    enddefine;

    readitem() -> item;

    ;;; check for "define : <form>"
    if item == ":" then
        unless isword(readitem() ->> item) then
            Pdr_error(item, 1, 'define_form name not a word')
        endunless;

        ;;; Construct identifier name of item
        consword(define_prefix sys_>< item) -> p_idname;
        if is_sub_syntax_word(p_idname, "define") ->> p_idname then
            ;;; run declared define_form procedure
            p_idname();
            return
        elseif item == "define_form" then
            "define" -> subsyntax, readitem() -> item
        elseif item == "sub_syntax" then
            readitem() -> subsyntax, readitem() -> item
        elseif item == "pdr_valof" then
            true -> pdr_val, readitem() -> item
        else
            Pdr_error(item, 1, 'unknown define form')
        endif
    endif;

    ;;; check for "updaterof"
    if item == "updaterof" then
        readitem() -> item, true
    else
        false
    endif -> upd;

    ;;; check for "iconstant"
    if item == "iconstant" then
        if testdef pop_#_include_stack and weakref pop_#_include_stack then
            "lconstant"
        else
            "constant" :: proglist -> proglist;
            "global"
        endif -> item
    endif;

    ;;; check for "global"/"nonglobal"
    if item == "global" then
        readitem() -> item, sysGLOBAL
    elseif item == "nonglobal" then
        readitem() -> item, sysGLOBAL(%false%)
    else
        false
    endif -> globl;

    ;;; check for identifier declaration keyword
    if (Is_decl_keyword(item, true) ->> decl_pdr) == sysLOCAL then
        ;;; dlocal -- read procedure name with itemread
        if subsyntax then Pdr_error(0, df_err_ms) endif;
        ;;; check for "nonactive"
        if (itemread() ->> item) == "nonactive" then
            itemread() -> item, "nonactive"
        else
            false
        endif -> _nonactive;
        ;;; get procedure name/assign expr (does sysLOCAL -- p_idname
        ;;; false if dlocal expr)
        Make_dlocal_expr(item, _nonactive) -> (assign_name, p_idname)

    else
        if decl_pdr then readitem() -> item endif;

        ;;; check for identprops, etc
        Read_idprops(item, true) -> (idprops, item);

        if isnumber(idprops) and idprops /== 0
        ;;; special test for defining "="
        and item /== "=" then
            ;;; operator -- rearrange args in normal form
            item :: proglist -> proglist;
            listlength(Get_pdargs(false) ->> list) -> num_opargs;
            [%  if num_opargs == 1 then
                    Restarg(1, list)
                elseif num_opargs == 2 then
                    Restarg(2, list), Restarg(1, list)
                elseif num_opargs == 3 then
                    Restarg(2, list), Restarg(3, list), Restarg(1, list)
                else
                    Pdr_error(0, 'invalid operator arguments')
                endif
            %] nc_<> proglist -> proglist;
            readitem() -> item
        endif;

        ;;; get procedure name
        sys_read_path(item, false, false) ->> assign_name -> p_idname;
        unless isword(p_idname) then
            Pdr_error(p_idname, 1, 'procedure name not a word')
        endunless;

        if subsyntax then
            if idprops or upd then
                Pdr_error(0, df_err_ms)
            else
                if subsyntax == "define" then
                    consword(define_prefix sys_>< p_idname) ->> assign_name
                                                        -> p_idname;
                endif;
                "syntax" -> idprops
            endif
        endif;

        ;;; check whether an identifier declaration is required
        if upd and not(globl or decl_pdr)
        and Sys$-Vm$-Is_local_var(p_idname) ->> id then
            ;;; updater for existing declaration -- if existing is active
            ;;; assume updater for the nonactive value
            id!ID_IDENTPROPS _bitst _:M_ID_ACTIVE
        else
            ;;; call user procedure to declare identifier
            pop11_define_declare(p_idname, globl, decl_pdr, idprops);
            false
        endif -> _nonactive;

        if _nonactive or ispair(idprops) then
            Nonactive(assign_name) -> assign_name
        endif
    endif;

    ;;; get default pdprops for procedure (unless dlocal expr)
    if (p_idname ->> props) and not(pop_syntax_only) then
        if ispair(p_idname!W_DICT_NEXT) then
            ;;; word identifier -- use the base word
            fast_front(p_idname!W_DICT_NEXT) -> props
        endif;
        pop11_define_props(p_idname, props, upd) -> props
    endif;


    ;;; check for define name = ...
    proglist -> savepl;
    Try_with(props, false) -> (props, nargs);
    if nextreaditem() == "=" then readitem() -> pdr_val endif;
    unless pdr_val then savepl -> proglist endunless;

    lvars lconst = not(isprocedure(assign_name))
                    and (Sys$-Vm$-Passign_ident(assign_name) -> (,,));

    if pdr_val then
        ;;; define name = ...

        define lconstant comp_expr();
            if pop11_comp_expr_to([enddefine ;]) == ";" then
                pop11_need_nextitem("enddefine") ->
            endif
        enddefine;

        unless pdr_val = "=" then pop11_need_nextitem(";") -> endunless;
        if lconst then
            ;;; lconstant assignment -- must be evaluated at compile-time
            pop11_exec_compile(comp_expr, 2:10) -> item
        elseif popclosebracket == popclosebracket_exec
        and not(pop_syntax_only) then
            comp_expr(), sysEXECUTE() -> item
        else
            sysPUSHQ(props);        ;;; push props first
            comp_expr();
            sysCALL("dup");         ;;; (props, p, p)
            if nargs then
                sysCALL("dup");
                sysPUSHQ(nargs);
                sysSWAP(1, 2);
                -> sysCALL("pdnargs")
            endif;
            ;;; assign the value to the identifier/expr
            if upd then
                if isprocedure(assign_name) then
                    assign_name()
                else
                    sysPUSH(assign_name)
                endif -> sysCALL("updater")
            else
                -> if isprocedure(assign_name) then
                        assign_name()
                   else
                        sysPUSH(assign_name)
                   endif
            endif;
            ;;; then assign the pdprops
            -> sysCALL("pdprops");  ;;; will produce an error if not a procedure
            return
        endif;
        unless pop_syntax_only then
            if issimple(item) or item <@(w) _system_end then
                ;;; don't assign props if item in system in case it's
                ;;; nonwriteable, e.g. define foo = identfn enddefine;
                Sys$-Check_procedure(item)
            else
                props -> pdprops(item);
                if nargs then nargs -> pdnargs(item) endif
            endif
        endunless
    else
        ;;; produce procedure
        if lconst then false -> pop_syntax_only endif;
        pop11_comp_procedure([enddefine {end}], p_idname, props) -> item
    endif;

    if subsyntax then
        ;;; construct pair for sub-syntax word
        conspair(subsyntax, item) -> item
    endif;

    ;;; assign to identifier/expr
    if upd then sysUPASSIGN else sysPASSIGN endif(item, assign_name)
enddefine;

define syntax procedure;
    dlocal pdr_error_ms = def_error_ms;
    pop11_comp_procedure([endprocedure {end}], false, false) -> pop_expr_item;
    sysPUSHQ -> pop_expr_inst
enddefine;


;;; --- CONSTANT, LCONSTANT, LVARS, VARS & DLOCAL ---------------------------

define lconstant Do_decl_syntax(get_thing, rditem, lconst, sthead);
    lvars   item, last_was_id, attribute, attr_outer, tuple, idprops,
            init_list, lconst, thing, procedure (rditem, get_thing);
    dlvars  sthead;
    lconstant expr_closers = [, ) ; ^termin];

    define lconstant Err(mess);
        lvars mess;
        mishap((), sthead <> mess, 'pop11-decl:syntax')
    enddefine;

    define lconstant Quick_init(thing);
        lvars item, closer, thing, savepl = proglist, saveifs = PL #_if_stack;
        returnif(islist(thing)) (false);
        nextitem() -> item;
        unless PL _next_code _bitst (_:M_ID_IS_WORD _biset _:M_ID_TERMIN) then
            PL Chop();
            if fast_lmember(nextitem()->>closer, expr_closers) then
                ;;; init is a single item
                PL Chop();
                sysPASSIGN(item, thing);
                return(closer)
            endif
        endunless;
        savepl -> proglist, saveifs -> PL #_if_stack;
        false
    enddefine;

    local_init_list -> init_list;
    false ->> local_init_list ->> attribute ->> attr_outer ->> tuple
                                                    -> last_was_id;

    until (rditem() ->> item) == ";" or item == termin do
        if item == ")" then
            unless attribute or tuple then
                Err(0, 'UNEXPECTED ")"')
            elseif attribute == attr_outer then
                ;;; closing ) for tuple
                if not(last_was_id) then false :: tuple -> tuple endif;
                tuple -> thing;
                false ->> tuple -> attr_outer;
                true -> last_was_id;
                goto TRY_INIT
            else
                ;;; closing ) for attribute
                if tuple and not(last_was_id) then
                    false :: tuple -> tuple
                endif;
                false -> attribute;
                true -> last_was_id;
                nextloop
            endunless
        elseif item == "(" then
            unless tuple then
                if last_was_id then
                    Err(0, '"," REQUIRED BEFORE OPENING "(" OF TUPLE')
                endif;
                [] -> tuple;
                attribute -> attr_outer;
                nextloop
            endunless
        elseif item == "," then
            if tuple and not(last_was_id) then false :: tuple -> tuple endif;
            false -> last_was_id;
            nextloop
        elseif item == "=" then
            Err(0, 'UNEXPECTED "="')
        elseif tuple and last_was_id then
            Err(item, 1, 'EXPECTING "," OR ")"')
        endif;

        ;;; get thing to declare, etc
        if (get_thing(item, attribute) -> attribute ->> thing) == "(" then
            "(" :: proglist -> proglist;
            nextloop
        endif;
        true -> last_was_id;
        if tuple then
            thing :: tuple -> tuple;
            nextloop
        endif;

    TRY_INIT:
        if rditem == readitem then nextreaditem() else nextitem() endif;
        nextunless(() == "=");
        readitem() -> ;         ;;; remove the "="

        ;;; read initialisation expression

        if lconst then
            ;;; expression must be evaluated at compile-time
            unless Quick_init(thing) ->> item then
                ;;; pass this the 2:10 flag which allows delayed evaluation
                ;;; of pcrs referencing non-local lvars etc
                pop11_exec_compile(expr_closers, pop11_comp_expr_to, 2:11)
                                                            -> item;
                Plant_assign((), thing, true)
            endunless
        elseif init_list then
            ;;; Being called immediately after procedure header.
            ;;; In this case, must save the initialisation expression so that
            ;;; pop11_comp_procedure can deal with it after popping input
            ;;; args.
            ;;; First save the thing and proglist in a pair.
            conspair(thing, proglist) :: init_list -> init_list;
            ;;; now compile the expression in no-code mode
            procedure;
                ;;; no code-planting, and procedure formals treated as
                ;;; dummy idents because they may not yet be declared
                dlocal  pop_syntax_only = true,
                        pop_vm_dummy_idents = local_dummy_idents;
                pop11_comp_expr_to(expr_closers)
            endprocedure() -> item
        else
            lvars exec = popclosebracket == popclosebracket_exec;
            unless exec and (Quick_init(thing) ->> item) then
                pop11_comp_expr_to(expr_closers) -> item;
                Plant_assign((), thing, false)
            endunless;
            if exec then sysEXECUTE() endif
        endif;
        item :: proglist -> proglist
    enduntil;

    if tuple or attribute then Err(0, 'MISSING CLOSING ")"') endif;
    item :: proglist -> proglist;
    init_list -> local_init_list
enddefine;      /* Do_decl_syntax */


define pop11_comp_declaration(decl_p);
    dlvars sthead, procedure decl_p, globarg = false;

    define lconstant Read_ident(item, attribute) -> (item, attribute);
        lvars item, idprops = attribute, attribute;
        ;;; try for props
        unless idprops then
            if Read_idprops(item, true) -> item ->> idprops then
                if item == "(" then
                    idprops -> attribute;
                    returnif((readitem() ->> item) == "(")
                endif
            else
                0 -> idprops
            endif
        endunless;

        ;;; read identifier
        unless isword(sys_read_path(item, false, false) ->> item) then
            mishap(item, 1, ine_errms, ine_idstring)
        endunless;
        if item /== "_" then                ;;; ignore anon var
            decl_p(item, idprops);          ;;; declare it
            if globarg then
                if globarg == "nonglobal" then
                    sysGLOBAL(item, false)
                elseif globarg == "global"
                or pop_pop11_flags &&/=_0 POP11_PERM_GLOBAL then
                    sysGLOBAL(item)
                endif
            endif
        endif;
        if ispair(idprops) then Nonactive(item) -> item endif
    enddefine;

    if isword(decl_p) then (), decl_p -> (decl_p, globarg) endif;
    Sys$-Check_procedure(decl_p);

    if pdprops(caller(1)) ->> sthead then
        sthead sys_>< ':\s'
    else
        'pop11_comp_declaration:\s'
    endif -> sthead;

    Do_decl_syntax(Read_ident, readitem, decl_p==sysLCONSTANT, sthead)
enddefine;

define syntax lvars;
    pop11_comp_declaration(sysLVARS)
enddefine;

define syntax dlvars;
    pop11_comp_declaration(sysDLVARS)
enddefine;

define syntax lconstant;
    pop11_comp_declaration(sysLCONSTANT)
enddefine;

define syntax vars;
    if pop_vm_compiling_list /== []
    and pop_pop11_flags &&/=_0 POP11_VARS_CHECK then
        mishap(0, 'vars: STATEMENT NOT ALLOWED INSIDE PROCEDURE',
                                                invcontext_idstring)
    endif;
    pop11_comp_declaration(sysVARS, "undef")
enddefine;

define syntax constant;
    pop11_comp_declaration(sysCONSTANT, "undef")
enddefine;

define lconstant Do_global(globarg);
    lvars globarg, item = readitem();
    if item == "vars" then
        sysVARS
    elseif item == "constant" then
        sysCONSTANT
    else
        mishap(item, 1, 'NOT "vars" OR "constant" AFTER "global/nonglobal"',
                                                ica_idstring)
    endif;
    pop11_comp_declaration((), globarg)
enddefine;

define syntax global;
    Do_global("global")
enddefine;

define syntax nonglobal;
    Do_global("nonglobal")
enddefine;

define syntax iconstant;
    if testdef pop_#_include_stack and weakref pop_#_include_stack then
        ;;; being included -- behaves as lconstant
        sysLCONSTANT
    else
        ;;; being compiled -- behaves as global constant
        sysCONSTANT, "global"
    endif;
    pop11_comp_declaration()
enddefine;

define syntax weak;
    lvars item = readitem(), globarg = "undef";
    dlvars flags;
    if pop_vm_compiling_list /== [] then
        mishap(0, 'weak: STATEMENT NOT ALLOWED INSIDE PROCEDURE',
                                            invcontext_idstring)
    endif;
    if item == "global" then
         "global" -> globarg, readitem() -> item
    elseif item == "nonglobal" then
         "nonglobal" -> globarg, readitem() -> item
    endif;
    if item == "vars" then
        2:10 -> flags       ;;; bit 0 = 0 (vars), bit 1 = 1 (weak)
    elseif item == "constant" then
        2:11 -> flags       ;;; bit 0 = 1 (const), bit 1 = 1 (weak)
    else
        mishap(item, 1, 'NOT "vars" OR "constant" AFTER "weak"', ica_idstring)
    endif;
    pop11_comp_declaration( procedure(/*word, idprops*/);
                                sysSYNTAX((), flags);
                            endprocedure, globarg)
enddefine;


define syntax dlocal;

    define lconstant Read_expr(item, attribute) -> (item, attribute);
        lvars item, attr = attribute, attribute;
        if not(attr) and (item == "nonactive" or isinteger(item)) then
            item -> attr;
            if (itemread() ->> item) == "(" then
                attr -> attribute;
                returnif((itemread() ->> item) == "(")
            endif
        endif;

        Make_dlocal_expr(item, attr) -> (item, )
    enddefine;

    Do_decl_syntax(Read_expr, itemread, false, 'dlocal: ')
enddefine;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 25 1996
        Revised some error messages.
--- John Gibson, Apr 26 1996
        Added new syntax for go_on
--- John Gibson, Apr 11 1996
        "end" and "close" now in a vector for closing brackets (only
        recognised if declared syntax).
--- John Gibson, Apr  9 1996
        Added some mishap idstrings.
--- John Gibson, Mar  5 1996
        Made Compile_=? use SysPUSH_OR_IDENT from vm_plant.p
--- John Gibson, Jan 15 1996
        Made identifier declarations ignore new anonymous var "_"
--- John Gibson, Dec  9 1995
        Added matchvar syntax
--- John Williams, Nov 30 1995
        vars no longer issues a warning when used inside a procedure.
--- John Gibson, Jun 24 1995
        Changed list/vector constructors to use new sysCONSTRUCT (rather
        than sysEX*EC_OPTION_COMPILE)
--- John Gibson, May 23 1995
        # If POP11_OLD_VARS is clear in pop_pop11_flags, then
            (1) procedure formals now default to lvars;
            (2) nested defines now default to lconstant;
            (3) vars inside procedures produces a warning (although still a
                mishap if POP11_VARS_CHECK is set);
        # active is no longer required for updaterof of an active variable.
--- John Gibson, Aug 16 1993
        Test for Popc now pop_pas_mode == "popc"
--- John Gibson, Jun 18 1993
        Moved define_headers and define_terminators to pop11_proglist_util.p
--- John Gibson, Jun 17 1993
        Moved in ==> from pretty.p
--- John Gibson, Jun 15 1993
        Changed Do_decl_syntax to insist on a comma before the opening "("
        of a tuple following an identifier
--- John Gibson, Oct 31 1992
        Non-opener syntax words now initialised to pop_undef
--- John Gibson, Oct 27 1992
        Added default globalisation thru POP11_PERM_GLOBAL, and "nonglobal"
        syntax word to override it.
--- John Gibson, Oct 26 1992
        Quoted word syntax "ident foo" constructs word identifier for "foo"
--- John Gibson, Oct 16 1992
        Changes to weakref and testdef (weakref dependency vecs now dealt
        with in sys_use_current_ident)
--- John Gibson, Oct  8 1992
        Fixed Read_ident_path to allow a list following "weakref"
--- John Gibson, Sep 15 1992
        Stopped define foo = <system procedure> from assigning pdprops
--- John Gibson, Sep 17 1991
        Added iconstant
--- John Gibson, Apr 25 1991
        Using itemread for item following "with_nargs" etc now done with
        pop_autoload locally false.
--- John Gibson, Apr 12 1991
        Changed check on "with_nargs" value to just >= 0 rather than >= to
        number of formal args.
        Item following "with_nargs" or "with_props" now read with -itemread-
        to allow macros.
--- John Gibson, Nov  9 1990
        More tests for pop_syntax_only
--- Simon Nichols, Nov  8 1990
        Changed mishap codes to lower case.
--- John Gibson, Nov  5 1990
        More account taken of pop_syntax_only in some places
--- John Gibson, Sep 17 1990
        Rewrote code for compiling declarations and dlocal, adding `tuple'
        assignment syntax. Added `tuple' output local syntax to
        -pop11_comp_procedure-.
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Jul 19 1990
        #: ... :# renamed #| ... |#
--- John Gibson, Jul 17 1990
        Added new syntax #: ... :# for counted statement sequence
--- John Gibson, Jul  7 1990
        Replaced -C*ompile_time_eval- with -pop11_exec_compile-
--- John Gibson, Apr  9 1990
        Changed -Tryupd_xsqcomp- to use -Comp_upd_expr_seq- in update mode.
--- John Gibson, Aug  4 1989
        Fix problem in -pop11_comp_procedure- to do with reading declaration
        statements following procedure header (required introduction of
        -pop_vm_dummy_idents- in the VM).
--- John Gibson, Jun  6 1989
        Added new option :sub_syntax <name> ... to define.
--- John Gibson, Jun  2 1989
        Compiler-controlling variables popdefineconstant/procedure etc
        replaced with bits in -pop_pop11_flags-.
--- John Gibson, May 16 1989
        Optimised initialisations in declarations for the case where the
        initialising expression is a single (non-word) item.
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, May  9 1989
        pop_vm_compiling_list /== [] is now the appropriate test for being
        inside procedure compilation (since popexecute can now be false
        inside a non-executing lblock).
--- John Gibson, Apr 18 1989
        Changes to define:define_form.
--- Ian Rogers, Apr 14 1989
        Added -in_list- and -on_list- as nonopener syntax words with
        similar uses as -in- and -on-.
--- John Gibson, Apr 11 1989
        Added define <name> = <expr> enddefine (as replacement for
        define :pdr_valof, although the latter remains in for now).
--- John Gibson, Feb 17 1989
        Altered -dlocal- so that it uses -itemread- to read items rather than
        -readitem-, and thus expands macros.
            Allowed 'define dlocal ...' to take an expression %...% as the
        item being defined (i.e. as for a dlocal statement).
            Redid the code for 'define :pdr_valof ...' to that it works
        properly with all the keywords that a normal define would
        (e.g. lconstant, updaterof, nonactive, etc, etc).
--- John Gibson, Jan 29 1989
        Added -weak- syntax word, changes for new popc
--- John Gibson, Jan 22 1989
        Made ( syntax procedure free temporary lvars used
--- John Gibson, Jan 18 1989
        Change to -pop11_comp_procedure- to prevent undeclared formals
        getting autoloaded while reading declarations after the procedure
        header.
--- John Gibson, Nov 24 1988
        Removed "dlconstant" from -define_headers-.
--- John Gibson, Nov 23 1988
        Added -lblock- construct.
--- John Gibson, Nov  6 1988
        => syntax operator now tests popclosebracket == popclosebracket_exec
        to determine whether to print whole stack or not.
--- Aaron Sloman, Oct 18 1988
        Renamed Procedure_compile as pop11_comp_procedure and exported it.
        Gave pdr_error_ms a default global value.
--- John Gibson, Sep  2 1988
        Undid last change to -Procedure_compile- (it causes undeclared
        input or outputs referenced in the nested procedure to produce
        DECLARING VARIABLE warnings).
--- John Gibson, Aug 27 1988
        Made -Procedure_compile- treat -define- immediately after
        header like -lvars-, etc.
            Trailing ^^ in a list now calls -sysconslist_onto- (so that
        it works with POPC).
--- Aaron Sloman, Jul 17 1988
        Undid the "fix" below, and fixed the bug they were bugged by.
--- Ian Rogers & Andrew Casson, Jul 16 1988
        Moved check for user defined, or autoloadable, define:<form> to
        correct place. Also deleted declaration of define_define_form.
--- Aaron Sloman, Jul 10 1988
        changed define:<form> to autoload define_<form>
--- Aaron Sloman, Jul  8 1988
        changed define :procval to define :pdr_valof
--- Aaron Sloman, Jul  6 1988
        Extended definition of """ to allow "<string>"
        Moved define_headers and define_terminators in here, instead of
            vddefsrch.p
        Added mechanisms for define : define_form. (See HELP * DEFINE_FORM)
            Including define_prefix
--- John Gibson, May  1 1988
        Partial application syntax now uses -sysanyvecons- with -consclosure-
        rather than -partapply-.
--- John Gibson, Mar 27 1988
        -list_assoc_val- into section Sys
--- John Gibson, Mar  7 1988
        Renamed pop11_syntax.p (was syntax.p).
        -sysvecons- and -sysanyvecons- to vectors.p
--- John Gibson, Mar  6 1988
        Fixed bug where -nonmac- ignored after -vars- statement in procedure
--- John Gibson, Mar  4 1988
        Made -Procedure_compile- check for missing opening (.
--- John Gibson, Feb 28 1988
        -Special_idprops- into section Sys
--- John Gibson, Feb 26 1988
        Added -declare_strong- for syntax closers
--- John Gibson, Feb 22 1988
        Allowed for section pathname after ^ or ^^ in lists, etc
--- John Gibson, Feb  7 1988
        Modified -Read_ident_path- to not expand macros inside pathnames.
--- John Gibson, Jan 19 1988
        Modifications to weakref to allow dependency list
 */
