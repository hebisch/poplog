/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lisp/src/plant.p
 > Purpose:         Common Lisp VM instructions
 > Author:          John Williams, Oct 28 1986 (see revisions)
 > Documentation:
 > Related Files:   C.all/lisp/src/eval.p, C.all/lisp/src/tokens.p
 */

lisp_compile_mode;

section $-lisp;

fastprocs destpair, lmember, ncrev, frozval, repeat;

vars procedure (compile_form, top_level_loop);


defprop
    f_specials,
    is_global_special,
    global_var_type,
    ;


vars
    &_rest_var                  =   false,
    Blocks                      =   [],
    BoundSpecials               =   false,
    Comp_stklen                 =   0,
    Compilation_speed           =   1,
    Compiling                   =   false,
    Current_function_info       =   false,
    Debug                       =   1,
    Doing                       =   false,
    Exec_stklen                 =   0,
    Function_vars               =   [],
    Init_stklen                 =   0,      /* Also declared in lispcore.p */
    Lambda_count                =   0,
    Lambda_prefix               =   nullstring,
    Lexidents                   =   [],
    Lispcompiler                =   false,
    Local_var_types             =   [],
    Optimised                   =   false,
    Proclaiming                 =   false,
    Restart_label               =   false,
    Safety                      =   1,
    Space                       =   1,
    Specials                    =   [],
    Speed                       =   1,
    Stackoffset                 =   0,
    Stklenvar                   =   false,
    Symbol_macros               =   false,
    Tags                        =   false,
    Tail_recursion              =   1,
    Tmp_ftok_list,              ;;; initialised below
    Tmp_vtok_list,              ;;; initialised below
    Undefs                      =   false,
    ;


constant macro (
    Fast        =   [((Speed fi_- Safety) fi_>= 2)],
    Debugging   =   [(Debug == 3)],
    );


/* Some macros for binding useful sets of variables */

constant macro (
    INIT_DECLARE_VARS
        =  [dlocal
                &_rest_var              =   false,
                Compilation_speed,
                Debug,
                Function_vars,
                Local_function_tokens,
                Local_var_types,
                Safety,
                Space,
                Specials                = [],
                Speed,
                Tail_recursion,
                constant_functions,
                pop_vm_flags,
            ;],

    INIT_FCOMP_VARS
        =  [dlocal
                BoundSpecials           =   [],
                Current_function_info   =   false,
                Lambda_count,
                Lambda_prefix,
                Restart_label           =   false,
            ;],

    INIT_STK_VARS
        =  [dlocal
                Stklenvar               =   false,
                Stackoffset             =   0,
            ;],
    );


/* Symbol macros */

define symbol_macro(sym);
    Symbol_macros and list_assoc(sym, Symbol_macros)
enddefine;


/* Undefined variable/function warnings */

define lconstant Undef_warn(tok);
    lvars sym;
    if (isfunction_token(tok) ->> sym) then
        unless ft_function_info(tok) do
            warn('Undefined function ~S', [^sym]) ->
        endunless
    else
        unless is_global_special(tok) do
            warn('Undefined variable ~S', [^tok]) ->
        endunless
    endif
enddefine;


define save_undef_token(tok);
    if popexecute then
        return
    elseif Undefs then
        conspair({^tok ^popfilename ^poplinenum}, Undefs) -> Undefs
    else
        Undef_warn(tok)
    endif
enddefine;


/* Set up new execute level */

define lispCOMPILE(Lispcompiler);
    lvars save;
    dlocal
        Comp_stklen,
        Lispcompiler,
        Lambda_count            =   0,
        Lambda_prefix           =   nullstring,
        ;
    INIT_DECLARE_VARS;

    define lconstant Save_undefs();
        Undefs -> save;
        if dup(/* Lispcompiler */) == top_level_loop then
            false -> Undefs
        elseif Undefs == false then
            [] -> Undefs
        endif
    enddefine;

    define updaterof lconstant Save_undefs();
        lvars list, tok;
        dlocal popfilename, poplinenum;
        if not(save) and Undefs then
            ncrev(Undefs) -> list;
            save -> Undefs;
            until list == [] do
                explode(sys_grbg_destpair(list) -> list)
                    -> (tok, popfilename, poplinenum);
                unless isboundtoken(tok) do
                    Undef_warn(tok)
                endunless
            enduntil
        endif
    enddefine;

    dlocal 0 % Save_undefs() %;

    stacklength() -> Comp_stklen;
    until Lispcompiler() do
        setstacklength(Comp_stklen)
    enduntil
enddefine;


define lispEVAL(Doing);
    dlocal
        Blocks                  =   [],
        Compiling               =   false,
        Current_function_info   =   false,
        Doing,
        Exec_stklen,
        Local_function_tokens   =   [],
        Restart_label           =   false,
        Stackoffset             =   0,
        Stklenvar               =   "ident Exec_stklen",
        Tags                    =   false,
        Tmp_ftok_list,
        Tmp_vtok_list,
        pop_debugging           =   false,
        ;
    stacklength() -> Exec_stklen;
    compile_form(Doing, #_< consref(false) >_#);
    sysEXECUTE();
    stacklength() fi_- Exec_stklen
enddefine;


/* Optimize declarations */

define lispOPTIMISE(item, value);
    lvars id;
    if item == @DEBUG then
        "ident Debug"
    elseif item == @SAFETY then
        "ident Safety"
    elseif item == @SPEED then
        "ident Speed"
    elseif item == @SPACE then
        "ident Space"
    elseif item == @POPLOG:TAIL-RECURSION then
        "ident Tail_recursion"
    elseif item == @COMPILATION-SPEED then
        "ident Compilation_speed"
    else
        false
    endif -> id;
    if (id and isinteger(value) and value fi_>= 0 and value fi_<= 3) then
        if Proclaiming then
            set_global_valof(value, id)
        else
            value -> valof(id)
        endif;
        lconstant VM_FAST
            = VM_NO_BACK_JUMP_CHECKS _biset VM_NO_PDR_ENTRY_CHECKS;
        if Speed == 3 and Safety == 0 then
            false -> pop_debugging;
            pop_vm_flags _biset VM_FAST
        else
            pop_vm_flags _biclear VM_FAST
        endif -> pop_vm_flags;
        true
    else
        false
    endif
enddefine;


/* Dynamic-extent and type declarations */

define lispDYNAMIC_EXTENT(tok);
    lvars sym;
    if tok == &_rest_var then
        consref(&_rest_var) -> &_rest_var
    else
        advise('Ignoring DYNAMIC-EXTENT declaration for ~S',
                [%  if (isfunction_token(tok) ->> sym) then
                        [^@FUNCTION ^sym]
                    else
                        tok
                    endif
                %])
    endif
enddefine;


define has_procedure_ident(vtok);
    identtype(sys_use_current_ident(vtok) ->) == "procedure"
enddefine;


define lconstant Ensure_correct_idtype(vtok, old, new);
    lvars const;

    returnif(old == new);   /* BUG: function types may not be == */
    returnunless(is_declared_token(vtok));

    if isfunction_type(new)
    and identtype(vtok) /== "procedure" then
        re_declare_token(vtok, isconstant(vtok), false);
        if isconstant(vtok) == "undef" then
            valof(vtok) -> valof(vtok)
        endif
    elseif isfunction_type(old)
    and identtype(vtok) == "procedure" then
        valof(vtok);
        isconstant(vtok) -> const;
        syscancel(vtok);        /* 'cos some calls may already be compiled */
        re_declare_token(vtok, const, false);
        -> valof(vtok);
    endif
enddefine;


define lispTYPE(sym, type);
    lvars vtok, old;
    sv_token(sym) -> vtok;
    if Proclaiming then
        global_var_type(vtok) -> old;
        type -> global_var_type(vtok);
        Ensure_correct_idtype(vtok, old, type)
    else
        list_assoc(sym, Local_var_types) or global_var_type(vtok) -> old;
        if old
        and old /== type then
            [^@AND ^old ^type] -> type;
        endif;
        acons(sym, type, Local_var_types) -> Local_var_types
    endif
enddefine;


/* Creating variables and constants */

define global_special_p() with_nargs 1;
    is_global_special(sv_token())
enddefine;


define updaterof global_special_p(bool, sym);
    pop_true(bool) -> is_global_special(sv_token(sym))
enddefine;


define lispCONSTANT(sym, val);
    lvars vtok;
    sv_token(sym) -> vtok;
    if Proclaiming then
        returnif(isconstant(vtok) == true and valof(vtok) == val);
        re_declare_token(vtok, true, false);
        val -> valof(vtok)
    else
        /* N.B. this is un-documented
                (and also doesn't notice symbol macros)
        */
        sysLCONSTANT(vtok, 0);
        sysPASSIGN(val, vtok)
    endif
enddefine;


define lispCANCEL_LVAR(sym);
    lvars vtok, id;
    dlocal pop_vm_flags = (pop_vm_flags _biset VM_MIX_NONLOCAL_AND_LOCAL_LEX);
    if (sys_current_ident(sv_token(sym) ->> vtok) ->> id)
    and isident(id) /== "perm" then
        sysLVARS(vtok, false)
    endif
enddefine;


define lispSPECIAL(sym);
    lvars vtok;
    sv_token(sym) -> vtok;
    if Proclaiming then
        popfilename or true -> is_global_special(vtok)
    else
        if symbol_macro(sym) then
            program_error('Cannot declare symbol macro ~S as special', [^sym])
        endif;
        pushnew(sym, Specials) -> Specials;
        lispCANCEL_LVAR(sym)
    endif;
    sysdeclare(vtok)
enddefine;


define lispLOCAL(sym);
    lvars vtok;
    dlocal pop_vm_flags = (pop_vm_flags _biset VM_MIX_NONLOCAL_AND_LOCAL_LEX);
    if symbol_macro(sym) then
        acons(sym, false, Symbol_macros) -> Symbol_macros
    endif;
    sv_token(sym) -> vtok;
    if isconstant(vtok) or isprotected(vtok) then
        program_error('Cannot locally rebind constant ~S', [^sym])
    endif;
    if lmember(sym, Specials)
    or is_global_special(vtok) then
        lispCANCEL_LVAR(sym);
        sysLOCAL(vtok);
        if BoundSpecials then
            conspair(sym, BoundSpecials) -> BoundSpecials
        endif
    else
        if lmember(sym, Function_vars)
        or isfunction_type(global_var_type(vtok)) then
            sysLVARS(vtok, "procedure")
        else
            sysLVARS(vtok, 0)
        endif;
        if Debugging then
            sysPUSHQ(sym);
            sysIDENT(vtok);
            sysCALLQ(procedure() with_nargs 2;
                        conspair(conspair(Lexidents)) -> Lexidents
                     endprocedure)
        endif
    endif
enddefine;


/* Function declarations */

define lispFDECLARE(sym, flags);
    lvars ftok;
    if flags _bitst _FT_LOCAL then
        consfunction_token(sym) ->> ftok -> current_sf_token(sym);
        sysLCONSTANT(ftok, "procedure");
        sys_current_ident(ftok) -> fast_cont(ftok)
    else
        sf_token(sym) -> ftok;
        re_declare_token(ftok, pop_true(constant_functions), true)
    endif;
    ft_perm_flags(ftok) _biset flags -> ft_flags(ftok)
enddefine;


define lispFASSIGN(pdr, sym);
    pdr -> ft_valof(current_sf_token(sym))
enddefine;


vars no_user_inlines = false;       /* Undocumented debugging hook */

define deref(item);
    if isref(item) then fast_cont(item) else item endif
enddefine;


define lispINLINE(sym, bool);
    lvars ftok, flags, p;
    pop_true(bool) -> bool;
    if bool
    and no_user_inlines
    and symbol_package(sym) /== lisp_package then
        advise('Ignoring user inline declaration for ~S', [^sym]);
        return
    endif;
    current_sf_token(sym) -> ftok;
    ft_flags(ftok) -> flags;
    returnif(flags _bitst _FT_LOCAL);
    ft_compiler_macro(ftok) -> p;
    sysdeclare(ftok);
    unless Proclaiming then
        copy_function_token(ftok) ->> ftok -> current_sf_token(sym)
    endunless;
    if bool then
        deref(p) -> ft_compiler_macro(ftok);
        flags _biset _FT_INLINE
    else
        if isprocedure(p) then consref(p) else p endif
            -> ft_compiler_macro(ftok);
        unless lisp_system_building then
            isprotected(ftok) -> bool;
            sysunprotect(ftok);
            if bool then sysprotect(ftok) endif
        endunless;
        flags _biclear _FT_INLINE
    endif -> ft_flags(ftok)
enddefine;


/* Temporary tokens */

lvars
    Tmp_vtok_count = 0,
    Tmp_ftok_count = 0,
    ;

vars
    Tmp_vtok_list
        = pdtolist(procedure();
                        make_symbol('TMP-VTOK-' sys_>< Tmp_vtok_count);
                        Tmp_vtok_count fi_+ 1 -> Tmp_vtok_count
                   endprocedure),
    Tmp_ftok_list
        = pdtolist(procedure();
                        make_symbol('TMP-FTOK-' sys_>< Tmp_ftok_count);
                        consfunction_token();
                        Tmp_ftok_count fi_+ 1 -> Tmp_ftok_count
                   endprocedure),
    ;

define lispNEW_VTOK() -> vtok;
    dest(Tmp_vtok_list) -> Tmp_vtok_list -> vtok;
    sysLVARS(vtok, 0)
enddefine;

define updaterof lispNEW_VTOK() with_nargs 1;
    conspair(Tmp_vtok_list) -> Tmp_vtok_list
enddefine;


define lispNEW_FTOK() -> ftok;
    dest(Tmp_ftok_list) -> Tmp_ftok_list -> ftok;
    sysLVARS(ftok, "procedure")
enddefine;

define updaterof lispNEW_FTOK() with_nargs 1;
    conspair(Tmp_ftok_list) -> Tmp_ftok_list
enddefine;


/* Pushes and pops */

define sysPUSHQ_DISCOUNT_LEX() with_nargs 1;
    dlocal pop_vm_flags = pop_vm_flags _biset VM_DISCOUNT_LEX_PROC_PUSHES;
    sysPUSHQ()
enddefine;


define lispPUSHQ() with_nargs 1;
    sysPUSHQ()
enddefine;


define lconstant Push_token(tok);
    lvars id;
    dlocal lispprwarning = save_undef_token;
    if Safety == 3
    and (sys_use_current_ident(tok) -> (id, ),
         isident(id) == "perm"
         and isconstant(id) /== true)
    then
        sysPUSHQ(tok);
        sysCALLQ(token_valof)
    else
        sysPUSH(tok)
    endif
enddefine;


define lispPUSH(sym);
    lvars form;
    if (symbol_macro(sym) ->> form) then
        compile_form(form, 1)
    else
        Push_token(sv_token(sym))
    endif
enddefine;


define lispFT_PUSH() with_nargs 1;
    Push_token()
enddefine;


define lconstant Pop_token() with_nargs 1;
    dlocal lispprwarning = save_undef_token;
    sysPOP()
enddefine;


define lispFT_POP() with_nargs 1;
    dlocal
        pop_debugging   =   false,
        pop_vm_flags    =   (pop_vm_flags _biset VM_NO_TYPED_VAR_CHECKS),
        ;
    sysCALLQ(checkr_function);
    Pop_token()
enddefine;


define lispPOP(sym);
    lvars vtok;
    if symbol_macro(sym) then
        program_error('Illegal assignment to symbol macro ~S', [^sym])
    endif;
    sv_token(sym) -> vtok;
    if has_procedure_ident(vtok) then
        lispFT_POP(vtok)
    else
        Pop_token(vtok)
    endif
enddefine;


define lispLOCAL_POP() with_nargs 1;
    lispLOCAL(dup());
    lispPOP()
enddefine;


define lconstant Note_nresults_available(wanted, available);
    fastprocs idval;
    if isident(wanted) then
        if idval(wanted) == nil then
            available -> idval(wanted)
        elseif available /== idval(wanted) do
            false -> idval(wanted)
        endif
    endif
enddefine;


define lconstant Pushn(item, nresults, push_p);
    deref(nresults) -> nresults;
    Note_nresults_available(nresults, 1);
    unless nresults == 0 do
        fast_apply(item, push_p);
        if isinteger(nresults) then
            repeat nresults - 1 times sysPUSHQ(nil) endrepeat
        endif
    endunless
enddefine;


define lispPUSHNQ() with_nargs 2;
    Pushn(lispPUSHQ)
enddefine;


define lispPUSHN() with_nargs 2;
    Pushn(lispPUSH)
enddefine;


define lispFPUSHN(sym, nresults);
    Pushn(current_sf_token(sym), nresults, lispFT_PUSH)
enddefine;


/* Stacklength manipulation */

define lispSAVE_STKLEN(first);
    0 -> Stackoffset;
    sysNEW_LVAR() -> Stklenvar;
    sysCALLQ(stacklength);
    sysPOP(Stklenvar);
    if first and Debugging then
        sysPUSH(Stklenvar);
        sysLOCAL("ident Init_stklen");
        sysPOP("ident Init_stklen")
    endif
enddefine;


define lispSET_STKLEN(wanted, available);
    lvars var;
    deref(wanted) -> wanted;
    Note_nresults_available(wanted, available);
    if isinteger(wanted) then
        if isinteger(available) then
            available fi_- wanted
        elseif Stklenvar then
            conspair(Stklenvar, wanted fi_+ Stackoffset)
        else
            sysNEW_LVAR() ->> var;
            sysCALLQ(stacklength);
            unless wanted == 0 do
                sysPUSHQ(wanted);
                sysCALLQ(nonop fi_+)
            endunless;
            sysPOP(var)
        endif
    else
        wanted
    endif
enddefine;


define lispRESET_STKLEN(n);

    define lconstant sysERASENUM(n);
        returnif(n == 0);
        if n fi_> 7 then
            sysPUSHQ(n);
            sysCALLQ(erasenum)
        elseif n fi_> 0 then
            repeat n times sysERASE(0) endrepeat
        elseif n fi_>= -7 then
            repeat negate(n) times sysPUSHQ(nil) endrepeat
        else
            sysPUSHQ(nil);
            sysPUSHQ(negate(n));
            sysCALLQ(dupnum)
        endif
    enddefine;

    define lconstant sysSETSTKLEN(var, n);
        dlocal pop_debugging = (Space > Speed);
        sysPUSH(var);
        sysPUSHQ(n);
        if n fi_< 256 then
            sysCALLQ(Lisp_setstacklength)
        else
            sysCALLQ(nonop fi_+);
            sysCALLQ(setstacklength)
        endif
    enddefine;

    if isinteger(n) then
        sysERASENUM(n)
    elseif ispair(n) then
        sysSETSTKLEN(destpair(n))
    elseif isword(n) then
        sysSETSTKLEN(n, 0);
        conspair(n, pop_new_lvar_list) -> pop_new_lvar_list
    endif
enddefine;


define lispRESET_STKLEN_IF_NEEDED(stkinfo);
    if atom(stkinfo)
    or (fast_front(stkinfo) /== Stklenvar)
    or (fast_back(stkinfo) /== Stackoffset) then
        lispRESET_STKLEN(stkinfo)
    endif
enddefine;


/* Compiling functions, macros, etc. */

define lispPROCEDURE() with_nargs 1;
    sysPROCEDURE(0);
    [] -> BoundSpecials;
    if Debugging then
        sysLOCAL("ident Lexidents");
        sysPUSHQ([]);
        sysPOP("ident Lexidents")
    endif
enddefine;


define lispENDPROCEDURE() -> p;
    lvars v;

    if isref(&_rest_var) and Safety /== 3 then

        define lconstant Uw() with_props 0; enddefine;

        define updaterof lconstant Uw() with_props 0;
            lvars lab;
            sysPUSH("dlocal_context");
            sysPUSHQ(2);
            sysCALLQ(nonop fi_>);
            sysIFSO(sysNEW_LABEL() ->> lab);
            lispPUSH(fast_cont(&_rest_var));
            sysCALLQ(sys_grbg_list);
            sysLABEL(lab)
        enddefine;

        sysLOCAL(Uw)
    endif;

    sysENDPROCEDURE() -> p;

    if ispair(BoundSpecials)
    and Debugging then
        consvector(destlist(ncrev(BoundSpecials))) -> v;
        if is_protected_closure(p) then
            sysPUSHQ(v);
            sysPUSHQ_DISCOUNT_LEX(p);
            sysCALLQ(
                procedure(p) with_nargs 2;
                    while isclosure(p) do pdpart(p) -> p endwhile;
                    -> f_specials(p)
                endprocedure)
        else
            v -> f_specials(p)
        endif
    endif
enddefine;


define lispFCOMPILE(name, type, comp_pdr) -> pdr;
    INIT_FCOMP_VARS;
    INIT_STK_VARS;

    if type == @FUNCTION or type == @MACRO then
        if name then
            current_sf_token(name) -> Restart_label;
            0 -> Lambda_count;
            symbol_string(name) <> '-' -> Lambda_prefix
        else
            Lambda_count + 1 -> Lambda_count;
            Lambda_prefix <> 'LAMBDA-' sys_>< Lambda_count -> name
        endif
    else
        0 -> Lambda_count;
        symbol_string(type) <> '-EXPAND-' <> symbol_string(name) -> name;
        name <> '-' -> Lambda_prefix
    endif;

    new_function_info(name) -> Current_function_info;
    popfilename -> fni_file(Current_function_info);

    lispPROCEDURE(Current_function_info);
    if Restart_label then
        sysLABEL(Restart_label)
    endif;
    comp_pdr();
    lispENDPROCEDURE() -> pdr;

    unless is_protected_closure(pdr) do
        fni_min(Current_function_info) -> pdnargs(pdr)
    endunless
enddefine;


/* Local variable blocks */

define lispLET() with_nargs 1;
    if (/* has_specials */ or Debugging) then
        lispPROCEDURE('Let');
        true
    else
        sysLBLOCK(false);
        false
    endif
enddefine;


define lispENDLET() with_nargs 1;
    if (/* is procedure let */) then
        sysCALLQ(lispENDPROCEDURE())
    else
        sysENDLBLOCK()
    endif
enddefine;


/* Function calls */

define lispTRUE();
    sysCALLQ(lisp_true)
enddefine;


define lispGO_ON() with_nargs 2;
    dlocal
        pop_debugging   =   false,
        pop_vm_flags    =   (pop_vm_flags _biset VM_NO_CHECK_GO_ON_INT),
    ;
    sysPUSHQ(1);
    sysCALLQ(nonop fi_+);
    sysGO_ON()
enddefine;


define lispNARGS();
    sysPUSH("LISP_N_ARGS")
enddefine;


define updaterof lispNARGS(nargs);
    if nargs then
        sysPUSHQ(nargs)
    else
        ;;; nargs on stack at run time
    endif;
    sysPOP("LISP_N_ARGS")
enddefine;


define lispCHECK_NARGS(nargs, rest);
    lvars lab;
    unless (nargs == 0 and rest) or Fast do
        sysNEW_LABEL() -> lab;
        lispNARGS();
        sysPUSHQ(nargs);
        sysCALLQ(if rest then nonop fi_>= else nonop == endif);
        sysIFSO(lab);
        sysCALLQ(runtime_nargs_error);
        sysLABEL(lab)
    endunless
enddefine;


define lispCALLI(pdr, args, nresults) -> Optimised;
    dlocal Optimised;
    if (f_inline(pdr) ->> Optimised) then
        Optimised(args, nresults)
    endif
enddefine;


define lconstant Is_terminal_call(/* nresults */) with_nargs 1;
    isident(/* nresults */) and BoundSpecials == []
enddefine;


define lispCALLQ(pdr, nargs, nresults);
    if nargs then
        check_nargs(function_info(pdr), nargs)
    endif;
    if nargs
    and pdpart(pdr) == check_fmin_apply then
        frozval(1, pdr) -> pdr
    else
        nargs -> lispNARGS()
    endif;
    if pdpart(pdr) == lisp_true_apply then
        sysCALLQ(frozval(1, pdr));
        lispTRUE()
    elseif Tail_recursion == 3
    and Is_terminal_call(nresults) then
        sysPUSHQ(pdr);
        sysCALLQ(fast_chain)
    else
        sysCALLQ(pdr)
    endif;
    lispRESET_STKLEN(nresults)
enddefine;


define lispCALL(tok, nargs, nresults);      /* N.B. tok might be symbol */
    lvars fni;
    if nargs and (ft_function_info(tok) ->> fni) then
        check_nargs(fni, nargs)
    endif;
    nargs -> lispNARGS();
    if Tail_recursion > 1
    and Is_terminal_call(nresults) then
        if tok == Restart_label
        and isconstant(tok) then
            sysGOTO(tok)
        elseif Tail_recursion == 3 then
            sysPUSH(tok);
            sysCALLQ(fast_chain)
        else
            sysCALL(tok)
        endif
    else
        sysCALL(tok)
    endif;
    lispRESET_STKLEN(nresults)
enddefine;


/* Conditionals */

define lispIFNOT() with_nargs 1;
    sysPUSHQ(nil);
    sysCALLQ(nonop /==);
    sysIFNOT()
enddefine;


define lispIFSO() with_nargs 1;
    sysPUSHQ(nil);
    sysCALLQ(nonop /==);
    sysIFSO()
enddefine;


/* Structure access/update VM instruction */

define lconstant NTH(n, key, upd);
    dlocal pop_debugging = false;
    if key == pair_key then
        if n fi_<= 3 or (Space < Speed and n fi_<= 10) then
            repeat n - 1 times
                sysCALLQ(fast_back)
            endrepeat;
            fast_front
        else
            sysPUSHQ(n);
            sysSWAP(1, 2);
            fast_subscrl
        endif;
        if upd then sysUCALLQ() else sysCALLQ() endif
    elseif key == string_key then
        sysPUSHQ(n);
        sysSWAP(1, 2);
        if upd then sysUCALLQ else sysCALLQ endif(lisp_subscrs)
    else
        if upd then sysUFIELD_VAL else sysFIELD_VAL endif(n, key)
    endif
enddefine;


define lispNTH() with_nargs 2;
    NTH(false)
enddefine;


define lispU_NTH() with_nargs 2;
    NTH(true)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Sep  5 1996
        lispINLINE(false) does not unprotect the identifier if
        lisp_system_building is true.
--- John Williams, Sep  3 1996
        Fast and Debugging now macros instead of active variables (speeds
        up compilation of Lisp code).
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Jun  6 1995
        Added sysPUSHQ_DISCOUNT_LEX.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Feb 24 1995
        Fixed bug in Note_nresults_available.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, May 23 1994
        lispEVAL now calls compile_form with second argument <true> (to
        signify a top-level form).
--- John Williams, Apr 26 1994
        Symbol macros implemented.
        lispSPECIAL now calls sysdeclare, to stop spurious "Undefined
        variable" warnings. Also, saves source file in is_global_special.
        lispINLINE no longer automatically declares updaters inline.
        Added no_user_inlines (temporary debugging hack for Loom).
--- John Williams, Nov 30 1993
        lispINLINE now calls sysdeclare on the function token being
        declared (not)inline.
--- John Williams, Aug 26 1993
        Names generated for SETF or TYPE expander functions now include
        the string '-EXPAND-'
--- John Williams, Jul 12 1993
        Uses pop_true(constant_functions) instead of popdefineconstant
--- John Williams, Mar  3 1992
        Added -Compilation_speed- variable (not yet used)
        See BR rogere.52 and BR isl-fr.4415
 */
