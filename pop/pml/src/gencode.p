/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/pml/src/gencode.p
 > Purpose:         PML: Code generation
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */


section $-ml =>
    ml_compile_debug
;

;;; Forward declarations:
constant procedure (
    gen_match,
    gen_exp,
    gen_dec,
    gen_strdec,
);

vars
    ml_compile_debug = false,
        ;;; whether to compile in debug mode
;

lvars
    current_fn_id = false,
        ;;; identifier of the current function being compiled
    current_fn_start = false,
        ;;; start-label of the current function
    endmatch_label,
        ;;; end-label of the current match
    trace_code = false,
        ;;; whether to add tracing code
;


#_IF not(DEF val_optimiser)     ;;; allow for reloading

;;; val_optimiser:
;;;     can return a code-planting procedure for optimising calls to a var

define val_optimiser =
    val_ident <> newproperty([], 32, false, "tmparg");
enddefine;

#_ENDIF

;;; make_val_global, make_str_global, make_topdec:
;;;     export names from declarations. All bindings are done initially
;;;     to lexical identifiers; bindings which reach top-level must have
;;;     their values saved.

define lconstant make_val_global(val);
    lvars global_id, local_id, val, v;
    returnunless(val_idkind(val) == ID_LEX);
    mlPUSH(val);
    ID_VAL -> val_idkind(val);
    mlPOP(val);
    if val_tracing(val) then
        if isword(val_tracing(val)) then
            mlPUSH_TRACE(val), mlPOP_TRACE(val);
        endif;
    endif;
enddefine;

define lconstant make_str_global(str);
    lvars struct, str;
    str_env(str) -> struct;
    app(structenv_strs(struct), make_str_global);
    app(structenv_vals(struct), make_val_global);
enddefine;

define lconstant make_topdec(dec);
    lvars dec;
    if isValDec(dec) or isFunDec(dec) or isExceptionDec(dec) then
        app(first(dec), make_val_global);
    elseif isLocalDec(dec) or isLocalStrDec(dec) then
        app(second(dec), make_topdec);
    elseif isAbstypeDec(dec) then
        app(third(dec), make_topdec);
    elseif isStructureDec(dec) then
        app(first(dec), make_str_global);
    endif;
enddefine;


/*
 *  Pattern matching
 */

lconstant           ;;; kinds of pattern/match
    PM_BIND    = 1,
    PM_DETUPLE = 2,
    PM_SWITCH  = 3,
    PM_MIXED   = 4,
;

;;; new_fail_lab, use_fail_lab, is_used_fail_lab, plant_fail_lab:
;;;     used for labelling failure cases in matching code. Labels not used
;;;     are not planted, avoiding redundant code.

define lconstant new_fail_lab =
    consref(%false%);
enddefine;

define lconstant use_fail_lab(fail_lab) -> lab;
    lvars lab = Cont(fail_lab), fail_lab;
    unless lab then
        mlNEW_LABEL() ->> lab -> Cont(fail_lab);
    endunless;
enddefine;

lconstant procedure is_used_fail_lab = Cont;

define lconstant plant_fail_lab(fail_lab);
    lvars lab = Cont(fail_lab), fail_lab;
    unless lab then
        mishap(0, 'PLANTING UNUSED FAILURE LABEL');
    endunless;
    mlLABEL(lab);
enddefine;

;;; poptovar:
;;;     ensures that the match var -v- is a real variable (on the assumption
;;;     that it will be used more than once). Returns the variable name and
;;;     a flag indicating whether the variable is temporary, i.e. one already
;;;     marked as temporary (a ref) or one created specially for this pop.

define lconstant poptovar(v);
    lvars v;
    if isword(v) then
        false, v;
    elseif isref(v) then
        true, Cont(v);
    else
        mlPUSH(v), mlPOP(mlNEW_VAR() ->> v);
        true, v;
    endif;
enddefine;

;;; plant_constant_test:
;;;     compare the variable -v- against the constant -k- and go to -lab-
;;;     on failure.

define lconstant plant_constant_test(v, k, lab);
    lvars v, k, lab;
    mlPUSH(v), mlPUSHQ(k);
    mlCALL(if isinteger(k) then "==" else "=" endif);
    mlIFNOT(use_fail_lab(lab));
enddefine;

;;; plant_exception_test:
;;;     compare the variable -v- against the exception -exn- and go to -lab-
;;;     on failure.

define lconstant plant_exception_test(v, exn, lab);
    lvars v, exn, lab;
    mlPUSH(v);
    if val_arity(exn) == 0 then
        mlPUSH(exn), mlCALL("fast_back");   ;;; packet id
    else
        mlPUSHQ(1), mlPUSH(exn), mlCALL("fast_frozval");
    endif;
    mlCALL("=="), mlIFNOT(use_fail_lab(lab));
enddefine;

;;; plant_test_sequence:
;;;     select the case appropriate to -v- using sequential tests

define lconstant plant_test_sequence(v, vs, cases, default, plant_test);
    lvars v, cases, match, k, vs, lab, default, procedure plant_test;
    repeat
        Destpair(Destpair(cases) -> cases) -> match -> k;
        if cases == [] then
            ;;; last case
            plant_test(v, k, default);
            gen_match(vs, match, default);
            quitloop;
        else
            ;;; not the last case: pop the argument to a variable, but a
            ;;; temporary one where possible
            if poptovar(v) -> v then freevar(v) endif;
            plant_test(v, k, new_fail_lab() ->> lab);
            gen_match(vs, match, default);
            mlGOTO(endmatch_label);
            plant_fail_lab(lab);
        endif;
    endrepeat;
enddefine;

;;; plant_go_on:
;;;     select the case appropriate to -v- using a "go_on" instruction.
;;;     All the cases must be labelled with integer tags.

define lconstant plant_go_on(v, vs, cases, default);
    lvars i, case, lab, labs, v, vs, cases, default, default_lab;

    define lconstant case_<=(c1, c2);
        lvars c1, c2;
        Front(c1) fi_<= Front(c2);
    enddefine;

    use_fail_lab(default) -> default_lab;
    ;;; sort cases by their leading integer tags
    syssort(cases, false, case_<=) -> cases;
    ;;; generate case labels for a go_on
    [%  1 -> i;
        For case in cases do
            until i == Front(case) do
                ;;; fill in missing cases with the default label
                default_lab;
                i fi_+ 1 -> i;
            enduntil;
            mlNEW_LABEL();
            i fi_+ 1 -> i;
        endfor
    %] -> labs;
    ;;; plant the go_on
    mlPUSH(v), mlGO_ON(labs, default_lab);
    ;;; plant the labels
    For lab in labs do
        unless lab == default_lab then
            mlLABEL(lab);
            Destpair(cases) -> cases -> case;
            gen_match(vs, Back(case), default);
            mlGOTO(endmatch_label);
        endunless;
    endfor;
enddefine;

define lconstant plant_const_switch =
    plant_test_sequence(% plant_constant_test %);
enddefine;

define lconstant plant_exn_switch(v, vs, cases, default);
    lvars cases, v, vs, default;
    poptovar(v) -> v -> ;
    conspair(conspair(v, "fast_front"), vs) -> vs;
    conspair(v, MLWID(packet_id)) -> v;
    plant_test_sequence(v, vs, cases, default, plant_exception_test);
enddefine;

define lconstant plant_boolean_switch(b, vs, cases, default);
    lvars cases, match, b, vs, lab, default;
    Back(Destpair(cases) -> cases) -> match;
    if cases == [] then
        ;;; only one case
        use_fail_lab(default) -> lab;
    else
        mlNEW_LABEL() -> lab;
    endif;
    if b then mlIFNOT(lab) else mlIFSO(lab) endif;
    gen_match(vs, match, default);
    returnif(cases == []);
    Back(Front(cases)) -> match;
    mlGOTO(endmatch_label);
    mlLABEL(lab);
    gen_match(vs, match, default);
enddefine;

define lconstant plant_binary_switch(v, vs, cases, default);
    lvars v, vs, cases, default;
    mlPUSH(v);
    plant_boolean_switch(Front(Front(cases)), vs, cases, default);
enddefine;

define lconstant plant_list_switch(v, vs, cases, default);
    lvars v, vs, cases, default;
    mlPUSH(v), mlPUSH("nil"), mlCALL("==");
    plant_boolean_switch(Front(Front(cases)) == nil, vs, cases, default);
enddefine;

define lconstant plant_con_switch(rep, v, vs, cases, default);
    lvars v, vs, cases, default, rep, reuse;

    ;;; test whether any case uses the constructor argument
    define lconstant arg_used(cases);
        lvars rule, case, cases;
        For case in cases do
            For rule in Back(case) do
                returnunless(isWildCardPat(Front(rule)))(true);
            endfor;
        endfor;
        false;
    enddefine;

    go_on rep to
        error t_bool t_enum error t_ref t_list t_binary t_user
    else
        error;

    t_bool:
        conspair("dummy", vs) -> vs;
        plant_binary_switch(v, vs, cases, default);
        return;

    t_enum:
        conspair("dummy", vs) -> vs;
        if listlength(cases) fi_< GO_ON_THRESHOLD then
            plant_const_switch(v, vs, cases, default);
        else
            plant_go_on(v, vs, cases, default);
        endif;
        return;

    t_ref:
        conspair(conspair(v, "ml_cont"), vs) -> vs;
        gen_match(vs, Back(Front(cases)), default);
        return;

    t_list:
        if arg_used(cases) then
            poptovar(v) -> v -> reuse;
            conspair(if reuse then consref(v) else v endif, vs) -> vs;
        else
            conspair("dummy", vs) -> vs;
        endif;
        plant_list_switch(v, vs, cases, default);
        return;

    t_binary:
    t_user:
        if arg_used(cases) then
            poptovar(v) -> v -> reuse;
            conspair(if reuse then consref(v) else v endif, "fast_front"),
            conspair(vs) -> vs;
        else
            conspair("dummy", vs) -> vs;
        endif;
        conspair(v, "fast_back") -> v;
        if rep == T_BINARY then
            plant_binary_switch(v, vs, cases, default);
        elseif listlength(cases) fi_< GO_ON_THRESHOLD then
            plant_const_switch(v, vs, cases, default);
        else
            plant_go_on(v, vs, cases, default);
        endif;
        return;

    error:
        bad_data_rep(rep);

enddefine;

define lconstant constant_cases(match);
    lvars k, case, cases, pat, rule, match;
    returnif(match == [])([]);
    constant_cases(Back(match)) -> cases;
    Destpair(Front(match)) -> rule -> pat;
    first(pat) -> k;
    For case in cases do
        if Front(case) = k then
            conspair(rule, Back(case)) -> Back(case);
            return(cases);
        endif;
    endfor;
    conspair([^k ^rule], cases);
enddefine;

define lconstant exception_cases(match);
    lvars id, case, cases, pat, rule, match;
    returnif(match == [])([]);
    exception_cases(Back(match)) -> cases;
    Destpair(Front(match)) -> rule -> pat;
    if isExnPat(pat) then
        conspair(wildcard_node, rule) -> rule;
    else
        conspair(second(pat), rule) -> rule;
    endif;
    val_ident(first(pat)) -> id;
    For case in cases do
        if val_ident(Front(case)) == id then
            conspair(rule, Back(case)) -> Back(case);
            return(cases);
        endif;
    endfor;
    conspair([% first(pat), rule %], cases);
enddefine;

define lconstant constructor_cases(match);
    lvars tag, case, cases, pat, rule, match;

    define lconstant constructor_tag(/* con */) -> tag with_nargs 1;
        lvars tag;
        val_value(/* con */) -> tag;
        if ispair(tag) then
            Back(tag) -> tag;
        elseif isclosure(tag) then
            fast_frozval(1, tag) -> tag;
        endif;
    enddefine;

    returnif(match == [])([]);
    constructor_cases(Back(match)) -> cases;
    Destpair(Front(match)) -> rule -> pat;
    if isConPat(pat) then
        conspair(wildcard_node, rule) -> rule;
    else
        conspair(second(pat), rule) -> rule;
    endif;
    constructor_tag(first(pat)) -> tag;
    For case in cases do
        if Front(case) == tag then
            conspair(rule, Back(case)) -> Back(case);
            return(cases);
        endif;
    endfor;
    conspair([^tag ^rule], cases);
enddefine;

define lconstant plant_switch(v, vs, match, default);
    lvars item, v, vs, match, default;
    first(Front(Front(match))) -> item;
    if not(isval(item)) then
        plant_const_switch(v, vs, constant_cases(match), default);
    elseif val_isexn(item) then
        plant_exn_switch(v, vs, exception_cases(match), default);
    else
        plant_con_switch(val_datarep(item), v, vs, constructor_cases(match), default);
    endif;
enddefine;

define lconstant detuple(v, vs, match) -> (vs, match);
    lvars rule, pat, pats, i, v, vs, match, n = false, used = "none";

    For rule in match do
        Front(rule) -> pat;
        unless isWildCardPat(pat) then
            first(pat) -> pats;
            unless n then llength(pats) -> n endunless;
            1 -> i;
            For pat in pats do
                unless isWildCardPat(pat) or i == used then
                    if used == "none" then
                        i -> used;
                    else
                        "many" -> used;
                        quitloop(2);
                    endif;
                endunless;
                i fi_+ 1 -> i;
            endfor;
        endunless;
    endfor;

    if used == "many" then
        poptovar(v) -> v -> ;
        For rule on match do
            Front(Front(rule)) -> pat;
            if isWildCardPat(pat) then
                popstackmark;
                Repeat n times wildcard_node endrepeat;
                sysconslist_onto(Back(Front(rule)));
            else
                first(pat) <> Back(Front(rule));
            endif ->  Front(rule);
        endfor;
        if n == 2 then
            [% conspair(v,"fast_front"), conspair(v,"fast_back") %];
        else
            [% For i to n do conspair(v,i) endfor %]
        endif <> vs -> vs;
    elseif used == "none" then
        For rule on match do
            Back(Front(rule)) -> Front(rule);
        endfor;
    else
        For rule in match do
            Front(rule) -> pat;
            unless isWildCardPat(pat) then
                subscrl(used, first(pat)) -> Front(rule);
            endunless;
        endfor;
        if n == 2 then
            if used == 1 then "fast_front" else "fast_back" endif -> used;
        endif;
        conspair(conspair(v, used), vs) -> vs;
    endif;
enddefine;

define lconstant bind(varlist, v) -> v;
    lvars var, v, varlist;

    ;;; must deref if v is updatable
    define lconstant must_deref(v);
        lvars v;
        while ispair(v) do
            returnif(Back(v) == "ml_cont")(true);
            Front(v) -> v;
        endwhile;
        false;
    enddefine;

    ;;; make sure that -v- can't be reused after it's bound
    define lconstant make_permanent(v) -> (v, changed);
        lvars v, changed = false;
        if isref(v) then
            ;;; -v- is temporary: make it not so
            (Cont(v), true) -> (v, changed);
        elseif ispair(v) then
            if make_permanent(Front(v)) ->> changed then
                conspair((), Back(v)) -> v;
            else
                () -> ;
            endif;
        endif;
    enddefine;

    returnif(varlist == []);
    if must_deref(v) then
        poptovar(v) -> (, v);
    endif;
    if ispair(v) then
        ;;; -v- is a structure access: if it's to be used more than once,
        ;;; pop it to a new variable
        For var in varlist do
            if val_usage(var) fi_> 1 then
                poptovar(v) -> (, v);
                quitloop;
            endif;
        endfor;
    endif;
    make_permanent(v) -> (v,);
    For var in varlist do
        (ID_LEX, v) -> val_access(var);
    endfor;
enddefine;

define lconstant pat_kind(pat);
    lvars pat;
    while isLayeredPat(pat) do second(pat) -> pat endwhile;
    if isWildCardPat(pat) or isVarPat(pat) then
        PM_BIND;
    elseif isTuplePat(pat) then
        PM_DETUPLE;
    else
        PM_SWITCH;
    endif;
enddefine;

define lconstant match_kind(match);
    lvars match, kind, k;
    pat_kind(Front(Destpair(match) -> match)) -> kind;
    returnif(kind == PM_DETUPLE)(PM_DETUPLE);
    until match == [] do
        pat_kind(Front(Destpair(match) -> match)) -> k;
        returnif(k == PM_DETUPLE)(PM_DETUPLE);
        returnif(k /== kind)(PM_MIXED);
    enduntil;
    kind;
enddefine;

define lconstant find_switch(vs, match);
    lvars vs, match, col = 1;

    define lconstant is_switch(match);
        lvars rule, match;
        For rule in match do
            returnunless(pat_kind(Front(rule)) == PM_SWITCH)(false);
        endfor;
        true;
    enddefine;

    repeat
        returnif(is_switch(match))(col);
        Back(vs) -> vs;
        returnif(vs == [])(false);
        map(match, Back) -> match;
        col fi_+ 1 -> col;
    endrepeat;
enddefine;

define lconstant split_match(match);
    lvars match, kind, rule;
    Destpair(match) -> match -> rule;
    pat_kind(Front(rule)) -> kind;
    [%  rule;
        while pat_kind(Front(Front(match))) == kind do
            Destpair(match) -> match;
        endwhile;
    %], match;
enddefine;

;;; gen_match:
;;;     plants code to match -match- against a list of arguments -vs-

define gen_match(vs, match, default);
    lvars   vs, match, v, col, kind, rule, reuse, fail_lab, pat, default;
    dlocal  new_var_list;

    until vs == [] do
        if find_switch(vs, match) ->> col then
            PM_SWITCH -> kind;
            unless col == 1 then
                rotate(col, map(vs, identfn)) -> vs;
                For rule on match do
                    rotate(col, Front(rule)) -> Front(rule);
                endfor;
            endunless;
        else
            match_kind(match) -> kind;
        endif;
        Destpair(vs) -> vs -> v;
        if kind == PM_MIXED then
            conspair(poptovar(v), vs) -> vs -> reuse;
            new_fail_lab() -> fail_lab;
            gen_match(vs, split_match(match) -> match, fail_lab);
            returnunless(is_used_fail_lab(fail_lab));
            mlGOTO(endmatch_label);
            plant_fail_lab(fail_lab);
            if reuse then consref(Front(vs)) -> Front(vs) endif;
        else
            bind([%
                For rule in match do
                    Front(rule) -> pat;
                    while isLayeredPat(pat) do
                        poptovar(v) -> (, v);
                        first(pat);             ;;; var for binding
                        second(pat) -> pat;
                    endwhile;
                    if isVarPat(pat) then
                        first(pat);             ;;; var for binding
                        wildcard_node -> pat;
                    endif;
                    pat -> Front(rule);
                endfor;
            %], v) -> v;
            if kind == PM_SWITCH then
                plant_switch(v, vs, match, default);
                return;
            elseif kind == PM_DETUPLE then
                detuple(v, vs, match) -> (vs, match);
            else
                For rule on match do
                    Back(Front(rule)) -> Front(rule);
                endfor;
            endif;
        endif;
    enduntil;
    if Front(match) == [] then
        new_var_list;
    else
        gen_exp(Front(Front(match)));
    endif;
enddefine;

define lconstant alloc_matchvars(match, arity) -> matchvars;
    lvars vs, rule, match, arity, matchvars, allocated = 0;
    [% Repeat arity times false endrepeat %] -> matchvars;
    For rule in match do
        matchvars -> vs;
        until vs == [] do
            unless Front(vs) or isWildCardPat(Front(rule)) then
                mlNEW_VAR() -> Front(vs);
                allocated fi_+ 1 -> allocated;
                returnif(allocated == arity);
            endunless;
            Back(vs) -> vs;
            Back(rule) -> rule;
        enduntil;
    endfor;
enddefine;

define lconstant alloc_matchvar(pat);
    lvars pat;
    if isWildCardPat(pat) then false else mlNEW_VAR() endif;
enddefine;

define lconstant init_matchvar(v);
    lvars v;
    if v then mlPOP(v) else mlERASE(0) endif;
enddefine;

define lconstant gen_bind_match(vs, match);
    lvars   match, vs, fail_lab;
    dlocal  endmatch_label = false;
    new_fail_lab() -> fail_lab;
    gen_match(vs, match, fail_lab);
    if is_used_fail_lab(fail_lab) then
        mlGOTO(mlNEW_LABEL() ->> endmatch_label);
        plant_fail_lab(fail_lab);
        mlRAISE("Bind");
        mlLABEL(endmatch_label);
    endif;
    ;;; -gen_match- should have left the new var list on the stack
    () -> new_var_list;
enddefine;

define lconstant gen_fn_match(vs, match);
    lvars   match, vs, fail_lab;
    dlocal  endmatch_label, new_var_list;
    mlNEW_LABEL() -> endmatch_label;
    new_fail_lab() -> fail_lab;
    gen_match(vs, match, fail_lab);
    if is_used_fail_lab(fail_lab) then
        mlGOTO(endmatch_label);
        plant_fail_lab(fail_lab);
        mlRAISE("Match");
    endif;
    mlLABEL(endmatch_label);
enddefine;

define lconstant gen_handler_match(v, match, endmatch_label);
    lvars   match, v, fail_lab;
    dlocal  endmatch_label, new_var_list;
    new_fail_lab() -> fail_lab;
    gen_match([^v], match, fail_lab);
    if is_used_fail_lab(fail_lab) then
        mlGOTO(endmatch_label);
        plant_fail_lab(fail_lab);
        mlPUSH(v), mlCALL(MLWID(raise));
    endif;
enddefine;


/*
 *  Expressions
 */

;;; is_local_var_exp:
;;;     tests whether an expression is a VarExp bound to a local var
;;;     (suitable for use in a match). Returns the var if so.

define lconstant is_local_var_exp(e);
    lvars e;
    SWITCH e
    CASE VarExp(var) then
        lvars v;
        val_arity(var) fi_<= 1 and (val_access(var) -> v) == ID_LEX and v;
    else
        false;
    ENDSWITCH;
enddefine;

;;; selector:
;;;     creates a selector procedure for the field named -label- from
;;;     a record with fields -labels-

define lconstant selectors =
    newproperty([], 10, false, "perm");
enddefine;

define lconstant selector(label, labels) -> p;
    lvars p, i, label, labels;
    unless position(label, labels) ->> i then
        mishap(label, 1, 'UNDEFINED RECORD LABEL');
    endunless;
    if listlength(labels) == 2 then
        if i == 1 then fast_front else fast_back endif -> p;
    elseif selectors(i) ->> p then
        ;;; already found
    else
        sysPROCEDURE(false, 1);
            sysFIELD(i, conspair("full", false), false, false);
        sysENDPROCEDURE() ->> p -> selectors(i);
    endif;
enddefine;

;;; gen_explode:
;;;     explode a tuple of length n on the stack

define lconstant gen_explode(n);
    lvars i, n, v;
    dlocal new_var_list;
    if n == 2 then
        mlCALL("fast_destpair");
    else
        mlPOP(mlNEW_VAR() ->> v);
        For i to n do
            mlPUSHQ(i), mlPUSH(v), mlCALL("fast_subscrv");
        endfor;
    endif;
enddefine;

;;; gen_closure:
;;;     part apply a curried function var to a list of args (possibly
;;;     empty)

define lconstant gen_closure(var, args, ty);
    lvars var, args, ty;
    dlocal new_var_list;
    lvars name = val_name(var), arity = val_arity(var), env = initv(arity);
    define lconstant gen_layer(argno);
        lvars argno, arg;
        mlPROCEDURE(name, 1);
            mlPOP(mlNEW_VAR() ->> Subscrv(argno, env));
            if argno == arity then
                ;;; all args collected -- chain the procedure
                For argno to arity do
                    if isword(Subscrv(argno, env) ->> arg) then
                        mlPUSH(arg);
                    else
                        gen_exp(arg);
                    endif;
                endfor;
                if val_tracing(var) then mlPUSHQ(ty) endif;
                mlCHAIN(var);
            else
                ;;; return the procedure at run time
                gen_layer(argno fi_+ 1);
            endif;
        mlENDPROCEDURE(false);
    enddefine;
    ;;; evaluate any partial arguments given
    lvars arg, argno = 1, part_apply = false;
    For arg in args do
        SWITCH arg
        CASE ConstExp(_) or ConExp(_) or ExnExp(_) then
            arg -> Subscrv(argno, env);
        else
            gen_exp(arg);
            true -> part_apply;
            false -> Subscrv(argno, env);
        ENDSWITCH;
        argno fi_+ 1 -> argno;
    endfor;
    if part_apply then
        mlPROCEDURE(name, 0);
        lvars i = argno;
        until i == 1 do
            i fi_- 1 -> i;
            unless Subscrv(i, env) then
                mlPOP(mlNEW_VAR() ->> Subscrv(i, env));
            endunless;
        enduntil;
    endif;
    gen_layer(argno);
    if part_apply then
        mlENDPROCEDURE(true);
    endif;
enddefine;

;;; gen_wrapper:
;;;     create the "wrapped" version of a function

define lconstant gen_wrapper(fn, ty);
    lvars fn, ty;
    if val_tupled(fn) then
        mlPROCEDURE(val_name(fn), 1);
            gen_explode(val_arity(fn));
            if val_tracing(fn) then mlPUSHQ(ty) endif;
            mlCHAIN(fn);
        mlENDPROCEDURE(false);
    else
        gen_closure(fn, [], ty);
    endif;
enddefine;

define lconstant gen_tuple_exp(es);
    lvars arity = llength(es), es;
    app(es, gen_exp);
    if arity == 2 then
        mlCALL("conspair");
    else
        mlPUSHQ(arity), mlCALL("consvector");
    endif;
enddefine;

define lconstant gen_case_exp(e, match);
    lvars v, e, match;
    first(match) -> match;
    unless is_local_var_exp(e) ->> v then
        gen_exp(e);
        init_matchvar(alloc_matchvar(Front(Front(match))) ->> v);
    endunless;
    gen_fn_match(conspair(v, []), match);
enddefine;

define lconstant gen_fn_exp(match);
    lvars   match, vs;
    dlocal  current_fn_id = false, current_fn_start = false;
    mlPROCEDURE(false, 1);
        first(match) -> match;
        alloc_matchvars(match, 1) -> vs;
        init_matchvar(Front(vs));
        gen_fn_match(vs, match);
    mlENDPROCEDURE(false);
enddefine;

define lconstant gen_handle_exp(e, match);
    lvars e, match, pdr_id, lab;

    ;;; check_interrupt:
    ;;; dummy procedure to flush out any outstanding interrupts
    define lconstant check_interrupt(); enddefine;

    ;;; code for exception handler
    define lconstant gen_handler_code();
        lvars   v;
        dlocal  new_var_list;
        mlNEW_VAR() -> v;
        mlPOP(v);
        gen_handler_match(v, first(match), lab);
    enddefine;

    ;;; code for expression body
    mlNEW_FUN() -> pdr_id;
    mlPROCEDURE(false, 0);
        mlLOCAL(MLWID(exn_pdr)), mlPUSH(pdr_id), mlPOP(MLWID(exn_pdr));
        mlLOCAL(MLWID(exn_stacklength)), mlCALL("stacklength"), mlPOP(MLWID(exn_stacklength));
        gen_exp(e);
        mlCALLQ(check_interrupt);
        ;;; return <true> to indicate success
        mlPUSH("true");
    mlENDPROCEDURE(pdr_id);
    mlCALL(pdr_id);
    ;;; check for successful return
    mlIFSO(mlNEW_LABEL() ->> lab);
    gen_handler_code();
    mlLABEL(lab);
enddefine;

define lconstant gen_app_exp(e, args, lastcall);
    lvars e, args, lastcall;
    SWITCH e
    CASE VarExp(var) then
        ;;; applying a named function
        lvars fn = val_ident(var), arity = val_arity(var);
        lvars optimiser = val_optimiser(var);
        if val_isprimitive(var) then false -> lastcall endif;
        define lconstant gen_call(fn, lastcall, ty);
            lvars fn, lastcall, ty;
            if ty then
                ;;; push instance type for tracing
                if fn == current_fn_id then
                    ;;; recursive: use type of initial call which will
                    ;;; typically have less polymorphism than that given
                    mlPUSH("type");
                else
                    mlPUSHQ(ty);
                endif;
            endif;
            if not(lastcall) then
                mlCALL(fn);
            elseif fn == current_fn_id then
                mlGOTO(current_fn_start);
            elseif trace_code and current_fn_id then
                ;;; tracing the current function -- don't chain
                mlCALL(fn);
            else
                mlCHAIN(fn);
            endif;
        enddefine;
        if arity == 0 then
            ;;; unknown function
            if lastcall and Back(args) == [] then
                gen_exp(Front(args)), gen_call(fn, true, false);
                return;
            else
                lvars nargs = 0;
                repeat
                    Destpair(args) -> args;
                    nargs fi_+ 1 -> nargs;
                    quitunless(args /== [] and nonexpansive(Front(args)));
                endrepeat;
                Repeat nargs times gen_exp() endrepeat;
                mlCALL(fn);
                Repeat nargs fi_- 1 times mlCALLS(false) endrepeat;
            endif;
        elseif optimiser and optimiser(args) then
            ;;; a successful call to an optimising function will plant some
            ;;; code for the application and consume an appropriate number
            ;;; of args, leaving the rest on the stack
            () -> args;
        elseif val_tupled(var) then
            lvars arg;
            Destpair(args) -> (arg, args);
            SWITCH arg
            CASE TupleExp(es) then
                app(es, gen_exp);
            CASE ConstExp(es) then
                appdata(es, mlPUSHQ);
            else
                gen_exp(arg);
                gen_explode(arity);
            ENDSWITCH;
            gen_call(fn, lastcall and args == [], val_tracing(var) and nodetype(e));
        elseif arity fi_<= llength(args) then
            ;;; curried function, all args present
            Repeat arity times
                gen_exp(Destpair(args) -> args);
            endrepeat;
            gen_call(fn, lastcall and args == [], val_tracing(var) and nodetype(e));
        else
            ;;; curried function, with some args missing
            gen_closure(var, args, nodetype(e));
            return;
        endif;
    CASE ConExp(con) then
        gen_exp(Destpair(args) -> args);
        val_value(con) -> con;
        if isclosure(con) and pdpart(con) == conspair then
            mlPUSHQ(frozval(1, con)), mlCALL("conspair");
        elseif con /== identfn then
            mlCALLQ(con);
        endif;
    CASE ExnExp(exn) then
        gen_exp(Destpair(args) -> args);
        mlCALL(exn);
    CASE SelectorExp(lab, labs) then
        ;;; plant inline code for field selection
        lvars pos;
        unless position(lab, labs) ->> pos then
            mishap(lab, labs, 2, 'SELECTOR FAILURE: UNDEFINED LABEL');
        endunless;
        if listlength(labs) == 2 then
            gen_exp(Destpair(args) -> args);
            mlCALL(if pos == 1 then "fast_front" else "fast_back" endif);
        else
            mlPUSHQ(pos);
            gen_exp(Destpair(args) -> args);
            mlCALL("fast_subscrv");
        endif;
    CASE ConstExp(fn) then
        ;;; constant function evaluated by -pregen-
        gen_exp(Destpair(args) -> args);
        mlCALLQ(fn);
    else
        gen_exp(e);
    ENDSWITCH;
    ;;; reduce any remaining arguments one at a time
    until args == [] do
        gen_exp(Destpair(args) -> args);
        mlAPPLY();
    enduntil;
enddefine;

define gen_exp(e);
    lvars e;
    SWITCH e
    CASE ConstExp(k) then
        mlPUSHQ(k);
    CASE VarExp(var) then
        if val_arity(var) fi_> 1 or val_tracing(var) then
            ;;; unwrapped or traced function - push a wrapped version
            gen_wrapper(var, nodetype(e));
        else
            mlPUSH(var);
        endif;
    CASE ExnExp(exn) then
        mlPUSH(exn);
    CASE TupleExp(es) then
        gen_tuple_exp(es);
    CASE SeqExp(es) then
        until (Destpair(es) ->> es) == [] do
            gen_exp(), mlERASE(0);
        enduntil;
        gen_exp();
    CASE SelectorExp(lab, labs) then
        mlPUSHQ(selector(lab, labs));
    CASE LetExp(decs, e) then
        app(decs, gen_dec), gen_exp(e)
    CASE AppExp(fn, args) then
        gen_app_exp(fn, args, nodeprops(e));
    CASE AndExp(e1, e2) then
        lvars label = mlNEW_LABEL();
        gen_exp(e1);
        mlAND(label);
        gen_exp(e2);
        mlLABEL(label);
    CASE OrExp(e1, e2) then
        lvars label = mlNEW_LABEL();
        gen_exp(e1);
        mlOR(label);
        gen_exp(e2);
        mlLABEL(label);
    CASE IfExp(e1, e2, e3) then
        lvars else_label = mlNEW_LABEL(), end_label = mlNEW_LABEL();
        gen_exp(e1);
        mlIFNOT(else_label);
        gen_exp(e2);
        mlGOTO(end_label);
        mlLABEL(else_label);
        gen_exp(e3);
        mlLABEL(end_label);
    CASE WhileExp(e1, e2) then
        lvars loop_label = mlNEW_LABEL(), end_label = mlNEW_LABEL();
        mlLABEL(loop_label);
        gen_exp(e1);
        mlIFNOT(end_label);
        gen_exp(e2), mlERASE(0);
        mlGOTO(loop_label);
        mlLABEL(end_label);
        mlPUSHQ(ml_unit);
    CASE CaseExp(e, match) then
        gen_case_exp(e, match);
    CASE FnExp(match) then
        gen_fn_exp(match);
    CASE RaiseExp(e) then
        gen_exp(e), mlCALL(MLWID(raise));
    CASE HandleExp(e, match) then
        gen_handle_exp(e, match);
    ENDSWITCH;
enddefine;


/*
 * Value Declarations.
 */

define lconstant gen_valdec(valdec);
    lvars valdec;
    ;;; non-recursive bindings
    lvars vb, vbs = third(valdec);
    unless vbs == [] then
        lvars (vs, pats) = ([], []);
        For vb in vbs do
            SWITCH vb
            CASE ValBind(pat, e) then
                SWITCH pat
                CASE VarPat(var) then
                    ;;; special case for simple binding
                    SWITCH e
                    CASE VarExp(evar) then
                        ;;; synonym -- copy dynamic attributes
                        explode(val_ident(evar)) -> explode(val_ident(var));
                    else
                        gen_exp(e);
                        (ID_LEX, mlNEW_VAR()) -> val_access(var);
                        mlPOP(var);
                    ENDSWITCH;
                else
                    lvars v;
                    unless is_local_var_exp(e) ->> v then
                        gen_exp(e);
                        init_matchvar(alloc_matchvar(pat) ->> v);
                    endunless;
                    conspair(v, vs) -> vs;
                    conspair(pat, pats) -> pats;
                ENDSWITCH;
            ENDSWITCH;
        endfor;
        unless vs == [] then
            gen_bind_match(vs, [^pats]);
        endunless;
    endunless;
    ;;; recursive bindings
    lvars vb, vbs = fourth(valdec);
    unless vbs == [] then
        ;;; pattern code must be planted first to make sure that all
        ;;; the variables are declared
        lvars lab1 = mlNEW_LABEL(), lab2 = mlNEW_LABEL(), lab3 = mlNEW_LABEL();
        mlGOTO(lab2);
        mlLABEL(lab1);
        lvars match = [% map(vbs, first) %];
        lvars vs = alloc_matchvars(match, llength(vbs));
        revapp(vs, init_matchvar);
        gen_bind_match(vs, match);
        mlGOTO(lab3);
        mlLABEL(lab2);
        For vb in vbs do gen_exp(second(vb)) endfor;
        mlGOTO(lab1);
        mlLABEL(lab3);
    endunless;
enddefine;


/*
 * Function Declarations.
 */

define lconstant gen_fundec(fundec);
    lvars fundec;
    define lconstant gen_fb(fb);
        lvars fb;

        define lconstant gen_trace_init(fn);
            lvars fn;
            if isword(val_tracing(fn)) then
                ;;; this is a traceable function: localise the list
                ;;; of names being traced and add to it any specific
                ;;; to this function
                mlLOCAL(MLWID(trace_names));
                lvars lab = mlNEW_LABEL();
                mlPUSH(val_tracing(fn)), mlIFNOT(lab);
                mlPUSH(val_tracing(fn)), mlCALL(MLWID(trace_add));
                mlLABEL(lab);
                ;;; nested functions aren't traceable
                1 -> trace_code;
            endif;
            ;;; allocate local vars for trace control
            mlLVARS("tracing", 0); mlLVARS("type", 0);
            ;;; check for trace
            lvars lab = mlNEW_LABEL();
            mlCALL(MLWID(trace_check)), mlPOP("tracing");
            mlPUSH("tracing"), mlIFNOT(lab);
            ;;; tracing: increment the depth
            mlLOCAL(MLWID(trace_depth));
            mlPUSH(MLWID(trace_depth)), mlPUSHQ(1), mlCALL("fi_+"),
                mlPOP(MLWID(trace_depth));
            mlLABEL(lab);
        enddefine;

        define lconstant gen_trace_entry();
            ;;; get type of application instance
            mlPOP("type");
            ;;; print trace entry message
            lvars lab = mlNEW_LABEL();
            mlPUSH("tracing"), mlIFNOT(lab);
            mlPUSHQ(current_fn_id), mlPUSH("type"), mlPUSH("true"),
                mlCALL(MLWID(trace_print));
            mlLABEL(lab);
        enddefine;

        define lconstant gen_trace_exit();
            ;;; print trace exit message
            lvars lab = mlNEW_LABEL();
            mlPUSH("tracing"), mlIFNOT(lab);
            mlPUSHQ(current_fn_id), mlPUSH("type"), mlPUSH("false"),
                mlCALL(MLWID(trace_print));
            mlLABEL(lab);
        enddefine;

        SWITCH fb
        CASE FunBind(fn, _, match) then
            lvars name = val_name(fn), arity = val_arity(fn);
            mlPROCEDURE(name, arity);
                dlocal trace_code, new_var_list, new_fun_list, new_exn_list;
                dlocal current_fn_id = val_ident(fn);
                dlocal current_fn_start = mlNEW_LABEL();
                ;;; initialise tracing
                if trace_code then gen_trace_init(fn) endif;
                ;;; plant the function start label
                mlLABEL(current_fn_start);
                ;;; establish lexical block for local definitions
                mlLBLOCK(false);
                ;;; print trace entry message
                if trace_code then gen_trace_entry() endif;
                ;;; allocate and initialise parameters
                lvars params = alloc_matchvars(match, arity);
                revapp(params, init_matchvar);
                ;;; generate matching code
                gen_fn_match(params, match);
                mlENDLBLOCK();
                ;;; print trace exit message
                if trace_code then gen_trace_exit() endif;
            mlENDPROCEDURE(current_fn_id);
        ENDSWITCH;
    enddefine;
    SWITCH fundec
    CASE FunDec(fns, _, fbs) then
        lvars fn;
        For fn in fns do
            if val_idkind(fn) == ID_NONE then
                (ID_LEX, mlNEW_FUN()) -> val_access(fn);
                if trace_code == true then
                    ;;; allocate a trace flag for the function
                    mlPUSH("false"), mlPOP(mlNEW_VAR() ->> val_tracing(fn));
                else
                    trace_code -> val_tracing(fn);
                endif;
            endif;
        endfor;
        app(fbs, gen_fb);
    ENDSWITCH;
enddefine;


/*
 * Abstype Declarations.
 */

define lconstant gen_abstypedec(abstypedec);
    lvars abstypedec;
    app(third(abstypedec), gen_dec);
enddefine;


/*
 * Exception Declarations.
 */

define lconstant gen_exceptiondec(exceptiondec);
    lvars eb, exn, exceptiondec;
    For eb in second(exceptiondec) do
        first(eb) -> exn;
        if val_idkind(exn) == ID_NONE then
            (ID_LEX, mlNEW_EXN()) -> val_access(exn);
        endif;
        if val_arity(exn) == 0 then
            mlPUSHQ(ml_unit);
            if third(eb) then
                mlPUSH(third(eb));
                mlCALL(MLWID(packet_id));
            else
                mlCALL(MLWID(new_exn_value)),
            endif;
            mlPUSHQ(val_name(exn));
            mlPUSHQ(val_printer(exn));
            mlCALL(MLWID(conspacket));
        else
            mlPUSH(MLWID(conspacket));
            if third(eb) then
                mlPUSHQ(1);
                mlPUSH(third(eb));
                mlCALL("fast_frozval");
            else
                mlCALL(MLWID(new_exn_value)),
            endif;
            mlPUSHQ(val_name(exn));
            mlPUSHQ(val_printer(exn));
            mlPUSHQ(3);
            mlCALL("consclosure");
        endif;
        mlPOP(exn);
    endfor;
enddefine;


/*
 * Local Declarations.
 */

define lconstant gen_localdec(localdec);
    lvars localdec;
    app(first(localdec), gen_dec);
    app(second(localdec), gen_dec);
enddefine;


/*
 *  Structure Declarations
 */

lconstant procedure gen_strexp;

define lconstant gen_fncapp(strexp);
    lvars fnc, strexp;

    define lconstant push_args(struct);
        lvars struct;
        if isstr(struct) then str_env(struct) -> struct endif;
        revapp(structenv_vals(struct),
            procedure(val);
                lvars val;
                unless val_isvalcon(val) then
                    if val_arity(val) fi_> 1 or val_tracing(val) then
                        ;;; unwrapped function - push a wrapped version
                        gen_wrapper(val, val_type(val));
                    else
                        mlPUSH(val);
                    endif;
                endunless;
            endprocedure);
        revapp(structenv_strs(struct), push_args);
    enddefine;

    define lconstant pop_results(struct);
        lvars val, struct;
        if isstr(struct) then str_env(struct) -> struct endif;
        app(structenv_strs(struct), pop_results);
        For val in structenv_vals(struct) do
            if val_idkind(val) == ID_LEX then
                ;;; replace functor locals with structure locals
                (ID_LEX, val_isexn(val) and mlNEW_EXN() or mlNEW_VAR())
                    -> val_access(val);
                if val_tracing(val) then mlPOP_TRACE(val) endif;
                mlPOP(val);
            endif;
        endfor;
    enddefine;

    first(strexp) -> fnc;
    gen_strexp(second(strexp));
    push_args(fnc_arg(fnc));
    mlFUNCTOR(Cont(fnc_code(fnc)));
    pop_results(fnc_env(fnc));
enddefine;

define lconstant gen_strexp(strexp);
    lvars strexp;
    if isGenStrexp(strexp) then
        app(second(strexp), gen_strdec);
    elseif isAppStrexp(strexp) then
        gen_fncapp(strexp);
    elseif isLetStrexp(strexp) then
        app(first(strexp), gen_strdec);
        gen_strexp(second(strexp));
    endif;
enddefine;

define lconstant gen_structuredec(structuredec);
    lvars strb, structuredec;
    For strb in second(structuredec) do
        gen_strexp(third(strb));
    endfor;
enddefine;

define lconstant gen_localstrdec(strdec);
    lvars strdec;
    app(first(strdec), gen_strdec);
    app(second(strdec), gen_strdec);
enddefine;


/*
 * Declarations (which includes all top-level evaluations).
 */

define gen_dec(dec);
    lvars dec;
    if isValDec(dec) then
        gen_valdec(dec);
    elseif isFunDec(dec) then
        gen_fundec(dec);
    elseif isAbstypeDec(dec) then
        gen_abstypedec(dec);
    elseif isExceptionDec(dec) then
        gen_exceptiondec(dec);
    elseif isLocalDec(dec) then
        gen_localdec(dec);
    else
        bad_syntax_tree(dec);
    endif;
enddefine;

define gen_strdec(dec);
    lvars dec;
    if isStructureDec(dec) then
        gen_structuredec(dec);
    elseif isLocalStrDec(dec) then
        gen_localstrdec(dec);
    else
        gen_dec(dec);
    endif;
enddefine;

define lconstant gen_fncb(fncb);
    lvars fnc, fncb, args = [];

    define lconstant pop_args(struct);
        lvars val, struct;
        if isstr(struct) then str_env(struct) -> struct endif;
        app(structenv_strs(struct), pop_args);
        For val in structenv_vals(struct) do
            unless val_isvalcon(val) then
                (ID_LEX, val_isvar(val) and mlNEW_VAR() or mlNEW_EXN())
                    -> val_access(val);
                mlPOP(val);
            endunless;
        endfor;
    enddefine;

    define lconstant push_results(struct);
        lvars val, struct;
        if isstr(struct) then str_env(struct) -> struct endif;
        For val in rev(structenv_vals(struct)) do
            if val_idkind(val) == ID_LEX then
                mlPUSH(val);
                if val_tracing(val) then mlPUSH_TRACE(val) endif;
            endif;
        endfor;
        revapp(structenv_strs(struct), push_results);
    enddefine;

    first(fncb) -> fnc;
    pop_args(fnc_arg(fnc));
    gen_strexp(fifth(fncb));
    push_results(fnc_env(fnc));
enddefine;

define lconstant gen_functordec(dec);
    lvars fncb, dec;
    For fncb in second(dec) do
        defer_code(fncb, gen_fncb) -> Cont(fnc_code(first(fncb)));
    endfor;
enddefine;

define lconstant gen_topdec(dec);
    lvars dec;
    if isFunctorDec(dec) then
        gen_functordec(dec);
    else
        gen_strdec(dec);
    endif;
enddefine;

define gencode(topdec);
    lvars   decs, topdec;
    dlocal  current_fn_id, current_fn_start,
            new_var_list, new_fun_list, new_exn_list,
            trace_code = ml_compile_debug,
            pop_debugging = #_< pop_debugging >_#;
    unless (pregen(topdec) ->> decs) == [] then
        init_lexids();
        mlLBLOCK(true);
        app(decs, gen_topdec);
        app(decs, make_topdec);
        mlENDLBLOCK();
    endunless;
enddefine;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul  3 1995
        Changed node-matching syntax to SWITCH/CASE/ENDSWITCH
--- Robert John Duncan, Dec 20 1994
        Added support for function call tracing
--- Robert John Duncan, Dec 20 1994
        Change to the handling of wrapped/unwrapped values: every function
        now has just a single unwrapped value; wrapped versions are
        generated afresh as required. Also, the arity of a tupled function
        now gives the actual number of arguments (rather than always 1) so
        arity > 1 is a sufficient test for where wrappers may be needed.
        Special case added for function synonyms (e.g. val f = S.g) so that
        the unwrapped version is properly propagated.
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Simon Nichols, Aug 28 1992
        Another bug fix in -bind-: don't discard singleton variables if
        they're updatable.
--- Robert John Duncan, Sep 27 1991
        Bug fix in -bind-: now removes temporary status from variables
        before binding.
--- Robert John Duncan, Sep 24 1991
        Fixed a bug in -gen_app_exp-: application of FnExp must be
        transformed (to CaseExp) by -pregen- to get the tail recursion
        right.
--- Simon Nichols, Jul  2 1991
        Fixed bug in restarting tupled functions.
--- Simon Nichols, Jun 26 1991
        Changed -bind- and -gen_match- not to allocate lvars for singleton
        variables.
--- Robert John Duncan, Feb 11 1991
        Exceptions, constructors and variables now represented by a common
        "val" record.
--- Robert John Duncan, Feb  4 1991
        Changes for new environment interface.
--- Robert John Duncan, Jan 21 1991
        Change to exception handling: handler code is now planted in-line,
        and the exception procedure returns a flag to indicate whether it
        should be executed.
--- Robert John Duncan, Jan 21 1991
        Parse tree pre-processed by -pregen-. This removes redundant nodes
        and does some work that used to be done here -- e.g. identifying
        tupled functions and tail-recursive applications.
        Major rewrite of pattern matching code: now does re-ordering of
        columns where appropriate. Assignments to initial match vars made
        explicit.
        Simplified interface to optimiser procedures: now return a flag to
        indicate whether any code was planted.
        Renamed -gencode_X- to -gen_X-.
--- Robert John Duncan, Nov  6 1990
        Changed pdprops of exception handler function to look better in an
        exception traceback.
--- Robert John Duncan, Oct 10 1990
        Added -pop_debugging- as local to -gencode-.
        Fixed not to chain certain primitive functions.
--- Robert John Duncan, Oct  9 1990
        Made exception handlers tail recursive.
--- Robert John Duncan, Oct  2 1990
        Added local structure declarations.
--- Robert John Duncan, Aug  8 1990
        Changed to use new -sysFIELD-.
--- Simon Nichols, Aug  6 1990
        Changed -gen_tupled_fun_wrapper- to plant -arity- calls to
        -fast_subscrv- rather than a single call to -destvector-.
--- Rob Duncan, Jun 22 1990
        Changed to call -mlCHAIN-.
        Replaced -unit- with -ml_unit-.
--- Simon Nichols, Jun 21 1990
        Changes to support optimisation of tupled functions. The changes
        are mainly to -gen_fundec- and -gen_app_exp-. New procedures
        -gen_tupled_fun_wrapper- and -transform_clauses- (which is
        local to -gen_fundec-) have been introduced.
--- Rob Duncan, Mar 13 1990
        Change to exception handler: no need to reset the stack length
        (now done by -raise-).
        Added lexical block in -gen_fb- so that a jump back to the
        function start label will do all the lexical-closure building it
        needs.
--- Rob Duncan, Jan 31 1990
        Replaced -fast_cont- with -ml_cont- for proper treatment of refs
        as identifiers.
        Fixed a bug in -gen_app_exp-: -e- substituted for -fn-
--- Rob Duncan, Sep 12 1989
        Changed -gen_topdec- to cope with TopDec sequences.
        Changed -fnc_code- to contain a reference, to maintain proper
        sharing of code between instances of a functor even when the
        instance is created before the code is generated.
 */
