/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/format_compile.p
 > Purpose:         Compiler for format_print control strings
 > Author:          John Williams, Oct 10 1995
 > Documentation:
 > Related Files:   LIB * FORMAT_PRINT
 */

#_TERMIN_IF DEF POPC_COMPILING

uses format_print;

section $-lisp$-fpr => format_compile;

define lconstant Write_string_between(string, i, j);
    fast_for i from i to j do
        cucharout(fast_subscrs(i, string))
    endfor
enddefine;


define lconstant Push_params(params, pdr);
    lvars item;
    fast_for item in_vector params do
        sysPUSHQ(item);
        unless isinteger(item) then
            sysCALL("ident transform_param")
        endunless
    endfor;
    fast_repeat pdnargs(pdr) fi_- datalength(params) times
        sysPUSHQ(false)
    endrepeat
enddefine;


define f_compiler =
    newproperty([], 32, false, "perm")
enddefine;


vars
    Cond_labs = false,
    Cond_stack = false,
    ;


define fcompile_range(f_string, f_str_index, f_str_len);
    lvars i, params, colon, at, pdr, comp_pdr;
    dlocal f_string, f_str_index, f_str_len, f_char,
           Cond_labs = false, Cond_stack = [];

    sysPROCEDURE(f_string, 0);
    sysLOCAL("ident f_colon");
    sysLOCAL("ident f_at");

    while (locchar(`~`, f_str_index, f_string) ->> i) do
        quitif(i fi_> f_str_len);

        if i fi_> f_str_index then
            sysPUSHQ(f_string);
            sysPUSHQ(f_str_index);
            sysPUSHQ(i fi_- 1);
            sysCALLQ(Write_string_between)
        endif;

        i -> f_str_index;
        parse_directive(true) -> (params, colon, at, pdr);

        if (f_compiler(f_char) ->> comp_pdr) then
            comp_pdr(params, colon, at, pdr)
        elseif isstring(pdr) then
            sysCALL("ident next_f_arg");
            sysPUSHQ(params);
            sysPUSHQ(colon);
            sysPUSHQ(at);
            sysPUSHQ(pdr);
            sysCALL("ident format_apply")
        else
            sysPUSHQ(colon);
            sysPOP("ident f_colon");
            sysPUSHQ(at);
            sysPOP("ident f_at");
            Push_params(params, pdr);
            sysCALLQ(pdr)
        endif;

        f_str_index fi_+ 1 -> f_str_index
    endwhile;

    if f_str_len fi_>= f_str_index then
        sysPUSHQ(f_string);
        sysPUSHQ(f_str_index);
        sysPUSHQ(f_str_len);
        sysCALLQ(Write_string_between)
    endif;

    sysENDPROCEDURE()
enddefine;


define global fcompile_string(string);
    fcompile_range(string, 1, datalength(string))
enddefine;


define format_compile(string);
    dlocal f_subsystem;
    if isword(string) then
        string -> f_subsystem;
        -> string
    endif;
    sysCOMPILE(string, fcompile_string)
enddefine;


/* Compiler procedures for ~( ~< ~{ ~[ ~; ~newline */

appdata('(<{[^;', f_proc <> erase);


procedure(params, colon, at, pdr);
    lvars i, j;
    fmatch_bracket(`(`, `)`) -> (i, j, , );
    sysPUSHQ(fcompile_range(f_string, i, j));
    sysPUSHQ(1);
    sysPUSHQ(false);
    sysPUSHQ(colon);
    sysPUSHQ(at);
    sysCALL("ident f_case")
endprocedure -> f_compiler(`(`);


procedure(params, colon, at, pdr);
    lvars clauses, first_clause_args;
    read_clauses(`<`, `>`) -> (clauses, first_clause_args, );
    if f_colon and f_subsystem == "lisp" then           ;;; ended with ~:>
        appdata({% f_lb_prefixes(clauses, first_clause_args, colon) %},
                sysPUSHQ);
        sysPUSHQ(at);
        sysCALL("ident f_lb")
    else
        Push_params(params, pdr);
        sysPUSHQ({% appdata(clauses, fcompile_string) %});
        sysPUSHQ(first_clause_args);
        sysPUSHQ(colon);
        sysPUSHQ(at);
        sysCALL("ident f_justify")
    endif
endprocedure -> f_compiler(`<`);


procedure(params, colon, at, pdr);
    lvars i, j, at_least1, sublists = colon;
    fmatch_bracket(`{`, `}`) -> (i, j, at_least1, );
    unless datalength(params) == 1 and params(1) == 0 do
        sysPUSHQ(fcompile_range(f_string, i, j));
        sysPUSHQ(i);
        sysPUSHQ(j);
        Push_params(params, pdr);
        sysPUSHQ(at_least1);
        sysPUSHQ(sublists);
        if at then
            sysCALL("ident f_loop")
        else
            sysCALL("ident next_f_arg");
            sysCALL("ident f_loop_args")
        endif
    endunless
endprocedure -> f_compiler(`{`);


procedure(params, colon, at, pdr);
    lvars i, j, lab, clauses, lab2;
    if at then
        fmatch_bracket(`[`, `]`) -> (i, j, , );
        sysNEW_LABEL() -> lab;
        sysCALL("ident next_f_arg");
        if f_subsystem == "lisp" then sysCALLQ(pop_true) endif;
        sysIFNOT(lab);
        sysPUSH("ident f_arg_index");
        sysPUSHQ(1);
        sysCALLQ(nonop fi_-);
        sysPOP("ident f_arg_index");
        sysCALLQ(fcompile_range(f_string, i, j));
        sysLABEL(lab)
    elseif colon then
        read_clauses(`[`, `]`) -> (clauses, , );
        unless datalength(clauses) == 2 do
            mishap(f_string, destvector(clauses) fi_+ 1,
                    'Two clauses needed in ~:[ .. ~] directive')
        endunless;
        sysNEW_LABEL() -> lab;
        sysCALL("ident next_f_arg");
        if f_subsystem == "lisp" then sysCALLQ(pop_true) endif;
        sysIFSO(lab);
        sysCALLQ(fcompile_string(clauses(1)));
        sysGOTO(sysNEW_LABEL() ->> lab2);
        sysLABEL(lab);
        sysCALLQ(fcompile_string(clauses(2)));
        sysLABEL(lab2)
    else
        {%  sysNEW_LABEL(), false, sysNEW_LABEL(), [], params %}
        ;;; GO_ON           ELSE   END       LAB_LIST  GO_ON_ARG
            -> Cond_labs;
        conspair(Cond_labs, Cond_stack) -> Cond_stack;
        sysGOTO(Cond_labs(1));
        sysLABEL(sysNEW_LABEL() ->> lab);
        conspair(lab, Cond_labs(4)) -> Cond_labs(4)
    endif
endprocedure -> f_compiler(`[`);



procedure(params, colon, at, pdr);
    lvars lab;
    if Cond_labs then
        sysGOTO(Cond_labs(3));      ;;; finish previous clause
        sysLABEL(sysNEW_LABEL() ->> lab);
        if colon then
            lab -> Cond_labs(2)
        else
            conspair(lab, Cond_labs(4)) -> Cond_labs(4)
        endif
    else
        misplaced_directive(`;`)
    endif
endprocedure -> f_compiler(`;`);


procedure() with_nargs 4;
    erasenum(4);
    if Cond_labs then
        sysGOTO(Cond_labs(3));      ;;; finish final clause
        sysLABEL(Cond_labs(1));
        if datalength(Cond_labs(5)) == 0 then
            sysCALL("ident next_f_arg");
        else
            Push_params(Cond_labs(5), f_proc(`[`))
        endif;
        sysPUSHQ(1);
        sysCALLQ(nonop fi_+);
        sysGO_ON(rev(Cond_labs(4)), Cond_labs(2) or Cond_labs(3));
        sysLABEL(Cond_labs(3));
        back(Cond_stack) -> Cond_stack;
        if ispair(Cond_stack) then
            front(Cond_stack)
        else
            false
        endif -> Cond_labs
    else
        misplaced_directive(`]`)
    endif
endprocedure -> f_compiler(`]`);


procedure(params, colon, at, pdr);
    lvars lab = sysNEW_LABEL();
    Push_params(params, pdr);
    sysCALL("ident f_escape_needed");
    sysIFNOT(lab);
    if Cond_labs then
        sysGOTO(Cond_labs(3))
    else
        sysCALL("ident f_escape")
    endif;
    sysLABEL(lab)
endprocedure -> f_compiler(`^`);


procedure(params, colon, at, pdr);
    apply_directive(params, colon, false, pdr); ;;; skips white space
    if at then
        sysPUSHQ(`\n`);
        sysCALL("cucharout")
    endif
endprocedure -> f_compiler(`\n`);


procedure(params, colon, at, pdr);
    misplaced_directive(f_char)
endprocedure
    ->> f_compiler(`)`) ->> f_compiler(`>`) -> f_compiler(`}`);


endsection;
