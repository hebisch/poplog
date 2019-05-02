/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/eval.p
 > Purpose:         Evaluation of Common Lisp expressions
 > Author:          John Williams, Jul 22 1987 (see revisions)
 > Documentation:
 > Related Files:
 */

lisp_compile_mode;

section $-lisp;

fastprocs front, back, destpair;


/* Macro expansion */

vars macroexpand_hook = @FUNCALL;


define lconstant Expand_macro(form, p);
    if macroexpand_hook == @FUNCALL then
        lisp_apply(form, Local_function_tokens, p, 2, 1)
    else
        lisp_apply(p, form, Local_function_tokens,
                    checkr_function(macroexpand_hook), 3, 1)
    endif;
    /* Returns expansion and flag */
    dup() /== form
enddefine;


define macroexpand1(form, env);
    lvars sym, val, p;
    dlocal Local_function_tokens = checkr_lexical_env(env);
    if ispair(form)
    and issymbol(front(form) ->> sym)
    and (is_macro(sym) ->> p) then
        lisp_true(Expand_macro(form, p))
    elseif issymbol(form)
    and (symbol_macro(form) ->> val) then
        val, true       /* Should maybe use macroexpand_hook */
    else
        form, nil
    endif
enddefine;


/* Compiler macros */

define compiler_macro_function(sym, env);
    dlocal Local_function_tokens;
    checkr_lexical_env(env) -> Local_function_tokens;
    lisp_true(deref(ft_compiler_macro(current_sf_token(sym))))
enddefine;


define updaterof compiler_macro_function(p, sym, env);
    lvars ftok;
    if pop_true(env) then
        lisp_error(
            'Cannot specify lexical environment when setting COMPILER-MACRO-FUNCTION',
            [^sym ^env])
    endif;
    sf_token(sym) -> ftok;
    checkr_function(p);
    if isref(ft_compiler_macro(ftok) ->> p) then
        -> fast_cont(p)
    else
        -> ft_compiler_macro(ftok)
    endif
enddefine;


define compiler_macroexpand1(form, env);
    lvars sym, p;
    dlocal Local_function_tokens = checkr_lexical_env(env);
    if ispair(form)
    and issymbol(front(form) ->> sym)
    and isprocedure(ft_compiler_macro(current_sf_token(sym)) ->> p) then
        lisp_true(Expand_macro(form, p))
    else
        form, nil
    endif
enddefine;


/* Compile code to evaluate a single expression */

constant procedure (compile_args, compile_lambda);


define vars compile_form(form, nresults);
    lvars sym, args, p, ftok, flag;
    dlocal Compiling;
RESTART:
    form -> Compiling;

    if atom(form) then
        if issymbol(form) then
            lispPUSHN(form, nresults)
        else
            lispPUSHNQ(form, nresults)
        endif;
        return
    endif;

    destpair(form) -> (sym, args);

    if sym starts_with @LAMBDA then
        compile_lambda(sym) -> p;
        goto CF
    elseif isprocedure(sym) then
        sym -> p;
        goto CF
    endif;

    current_sf_token(sym) -> ftok;

    unless Compilation_speed fi_> Speed
    or Compilation_speed == 3
    then
        if (ft_compiler_macro(ftok) ->> p)
        and isprocedure(p) then
            Expand_macro(form, p) -> (form, flag);
            if flag then
                fast_chain(form, nresults, compile_form)
            endif
        endif
    endunless;

    procedure();
        dlocal lispprwarning = save_undef_token;
        valof()
    endprocedure(ftok) -> p;

    go_on ft_flags(ftok) to
        ;;;     2:0000      2:0001      2:0010      2:0011
                /* S */     F           M           ERROR

        ;;;     2:0100      2:0101      2:0110      2:0111
                ERROR       IF          IM          ERROR

        ;;;     2:1000      2:1001      2:1010      2:1011
                ERROR       LF          LM          ERROR

        ;;;     2:1100      2:1101      2:1110      2:1111
                ERROR       ERROR       ERROR       ERROR

    else
        S
    ;

S:
    returnif(lispCALLI(p, args, nresults));
ERROR:
    ft_flags(ftok), ftok;
    _FT_FUNCTION -> ft_flags(ftok);
    procedure();
        dlocal pop_pr_radix = 2;
        mishap(2, 'SYSTEM ERROR - ILLEGAL FT_FLAGS VALUE');
    endprocedure();
    goto RESTART;
IM:
    unless Speed == 0 or macroexpand_hook /== @FUNCALL do
        returnif(lispCALLI(p, args, nresults))
    endunless;
LM:
M:
    returnif(Speed == 3 and lispCALLI(p, args, nresults));
    Expand_macro(form, p) -> (form, flag);
    if flag then
        goto RESTART
    else
        program_error('Macro expansion is EQ to original form', [])
    endif;
IF:
    unless Speed == 0 do
        returnif(lispCALLI(p, args, nresults));
        unless isundef(p) then goto CF endunless
    endunless;
LF:
F:
    if isconstant(ftok) == true then
        returnif(Speed == 3 and lispCALLI(p, args, nresults));
CF:     lispSET_STKLEN(nresults, f_results(p)) -> nresults;
        lispCALLQ(p, compile_args(args), nresults)
    else
        lispSET_STKLEN(nresults, f_results(ftok)) -> nresults;
        lispCALL(ftok, compile_args(args), nresults)
    endif
enddefine;


define compile_arg() with_nargs 1;
    compile_form(1);
    Stackoffset fi_+ 1 -> Stackoffset
enddefine;


define compile_args(forms) -> nargs;
    dlocal Stackoffset;
    Stackoffset -> nargs;
    until endp(forms) do
        compile_arg(destpair(forms) -> forms)
    enduntil;
    Stackoffset fi_- nargs -> nargs
enddefine;


define global eval() with_nargs 1;
    sysCOMPILE(lispEVAL) ->
enddefine;


/* Miscellaneous things */

constant procedure declare;


define parse_body(body, checkdoc, dodecs) -> (body, decs, doc);
    lvars form;
    false -> doc;
    [% while ispair(body) do
        front(body) -> form;
        if pop_true(checkdoc)
        and isstring(form)
        and ispair(back(body)) then
            destpair(body) -> (doc, body);
            false -> checkdoc
        elseif form starts_with @DECLARE then
            back(body) -> body;
            if pop_true(dodecs) then
                declare(back(form))
            else
                form
            endif
        else
            quitloop
        endif
    endwhile %] -> decs
enddefine;


define Quote(form, nresults);
    unless islistlength(form, 1) do
        program_error('QUOTE expects one argument', [])
    endunless;
    lispPUSHNQ(front(form), nresults)
enddefine;


constant procedure (compile_function);

define Function(form, nresults);
    lvars sym;
    unless islistlength(form, 1) do
        program_error('FUNCTION expects one argument', [])
    endunless;
    front(form) -> form;
    if (fname_sym(form, false) ->> sym) then
        lispFPUSHN(sym, nresults)
    elseif form starts_with @LAMBDA then
        lispPUSHNQ(compile_lambda(form), nresults)
    else
        lispPUSHNQ(compile_function(form), nresults)
    endif
enddefine;


/* Top level forms */

lconstant macro
    SET_EXEC = [lvars EXEC
                    = (isref(nresults) and popexecute and Blocks == []);],
    ;


define Progn(forms, nresults);
    lvars form;
    lconstant r0 = consref(0);
    if endp(forms) then
        lispPUSHNQ(nil, nresults)
    else
        SET_EXEC;
        repeat
            destpair(forms) -> (form, forms);
            quitif(endp(forms));
            compile_form(form, if EXEC then r0 else 0 endif);
            if EXEC then sysEXECUTE() endif
        endrepeat;
        compile_form(form, nresults)
    endif
enddefine;


define If(forms, nresults);
    lvars test, action, default, lab, endlab;
    SET_EXEC;
    destlist(forms) -> forms;
    if forms == 2 then
        nil
    elseunless forms == 3 do
        program_error('IF expects either two or three arguments', [])
    endif -> (test, action, default);

    if test == true then
        compile_form(action, nresults)
    elseif test == nil then
        compile_form(default, nresults)
    elseif EXEC then
        compile_form(test, #_< consref(1) >_#);
        if pop_true(sysEXECUTE()) then
            compile_form(action, nresults)
        else
            compile_form(default, nresults)
        endif
    else
        sysNEW_LABEL() -> lab;
        compile_form(test, 1);
        lispIFNOT(lab);
        compile_form(action, nresults);
        sysGOTO(sysNEW_LABEL() ->> endlab);
        sysLABEL(lab);
        compile_form(default, nresults);
        sysLABEL(endlab)
    endif
enddefine;


define Eval_when(forms, nresults);
    lvars when, discard, item;
    unless ispair(forms) do
        program_error('Missing situation specifier list in EVAL-WHEN form', [])
    endunless;
    destpair(forms) -> (when, forms);
    if endp(when) then
        warn('Empty situation specifier list in EVAL-WHEN form', []) ->
    else
        true -> discard;
        SET_EXEC;
        for item in_cl_list when do
            if item == @EVAL
            or item == @:EXECUTE then
                false -> discard
            elseif item == @LOAD
            or item == @:LOAD-TOPLEVEL then
                if EXEC then
                    false -> discard
                endif
            else
                unless item == @COMPILE
                or item == @:COMPILE-TOPLEVEL do
                    program_error('Unrecognised EVAL-WHEN situation specifier: ~S',
                            [^item])
                endunless
            endif
        endfor
    endif;
    if discard then
        lispPUSHNQ(nil, nresults)
    else
        Progn(forms, nresults)
    endif
enddefine;


define Load_time_value(forms, nresults);
    lvars form;
    unless ispair(forms) do
        program_error('Empty LOAD-TIME-VALUE form', [])
    endunless;
    destpair(forms) -> (form, forms);
    if ispair(forms) then
        back(forms) -> forms        /* Skip read-only-p flag */
    endif;
    unless endp(forms) do
        program_error('Excess forms in LOAD-TIME-VALUE form', [^forms])
    endunless;
    unless form starts_with @VALUES and islistlength(form, 2) do
        [^@VALUES ^form] -> form
    endunless;
    lispPUSHNQ(eval(form), nresults)
enddefine;


/* Does a form have a constant value ? */

define is_constant_value(form) -> form;
    lvars temp;
    if issymbol(form) then
        if (symbol_macro(form) ->> temp) then
            fast_chain(temp, is_constant_value)
        endif;
        if isconstant(sv_token(form) ->> temp) == true then
            valof(temp)
        else
            false
        endif -> form
    elseif ispair(form) then
        if islistlength(form, 2)
        and (destpair(form) -> form) == @QUOTE then
            front(form)
        else
            false
        endif -> form
    endif
enddefine;


define constantp() with_nargs 1;
    if is_constant_value() then true else false endif
enddefine;


/* Given a form that is supposed to evaluate to a function, compile_fn
    compiles that form and returns the VM instruction and argument
    (call_p and call_arg) needed to compile a call to the result.
*/

define compile_fn(fn) -> call_arg -> call_p;
    lvars tok;

    if isprocedure(fn) then
        fn
    elseif issymbol(fn)
    and (has_procedure_ident(sv_token(fn) ->> tok)
         or (isconstant(tok) == true and isprocedure(valof(tok))))
    then
        fn
    elseif fn starts_with @QUOTE
    and islistlength(fn, 2) then
        cadr(fn) -> fn;
        if fn starts_with @LAMBDA then
            sysCOMPILE(fn, compile_lambda)
        else
            sf_token(fname_sym(fn))
        endif
    elseif fn starts_with @FUNCTION
    and islistlength(fn, 2) then
        cadr(fn) -> fn;
        if fn starts_with @LAMBDA then
            compile_lambda(fn)
        else
            current_sf_token(fname_sym(fn))
        endif
    else
        compile_form(fn, 1);
        lispFT_POP(dup(lispNEW_FTOK()))
    endif -> call_arg;

    if isprocedure(call_arg) then
        lispCALLQ
    elseif isconstant(call_arg) == true then
        valof(call_arg) -> call_arg;
        lispCALLQ
    else
        lispCALL
    endif -> call_p
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Jun  5 1995
        Added Load_time_value.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, May 23 1994
        Progn now works correctly as top-level form. Related changes to If.
--- John Williams, Apr 26 1994
        Changes for symbol macros and function names.
        Bug fix: (EVAL-WHEN (LOAD) ...) is now evaluated.
--- John Williams, Feb 23 1994
        compile_fn no longer allows for function forms that evaluate to
        lambda expressions.
--- John Williams, Feb 23 1994
        parse_body no longer attempts to expand macros (Steele 1990 p217).
--- John Williams, Dec  2 1993
        Eval_when now copes with an empty situation list.
        compile_fn now copes with function names of the form (setf name).
 */
