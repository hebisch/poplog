/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/apply.p
 > Purpose:         Applying Lisp functions (run-time)
 > Author:          John Williams, Mar 29 1988 (see revisions)
 > Documentation:
 > Related Files:   C.all/lisp/src/functions.p
 */

lisp_compile_mode;

section $-lisp => make_pop11_procedure;


constant
    procedure (compile_lambda),
    call_arguments_limit    =  pop_max_int + 1,
    multiple_values_limit   =  pop_max_int + 1,
    ;


define compiled_function_p(item);
    isprocedure(item) and not(isarray(item))
enddefine;


define functionp() with_nargs 1;
    compiled_function_p()
enddefine;


define checkr_function(item) -> item;
    unless compiled_function_p(item) do
        if item starts_with @LAMBDA then
            compile_lambda(item)
        else
            valof(sf_token(fname_sym(item)))
        endif -> item
    endunless
enddefine;


/* Standard error functions */

define exec_non_func(u);
    lvars tok;
    dlocal LISP_N_ARGS;
    undefword(u) -> tok;
    lisp_cerror(
        'Execute ~*~S again',
        @UNDEFINED-FUNCTION,
        {^@:NAME ^(isfunction_token(tok) or tok) ^@:OPERATION ^@:EXECUTE});
    chain(valof(tok))
enddefine;


define exec_special_form(sym);
    fast_chain(
              @POPLOG:APPLYING-SPECIAL-FORM,
              {^@:NAME ^sym ^@:OPERATION ^('apply')},
              lisp_error);
enddefine;


/* Checking numbers of args */

vars trace_args trace_results;

define cons_arglist();
    conslist(LISP_N_ARGS)
enddefine;

define dest_arglist() with_nargs 1;
    destlist() -> LISP_N_ARGS
enddefine;


define lconstant Check_nargs(fni, nargs, procedure err_p, continue);
    lvars fmin, fmax;
    f_min(fni) -> fmin;
    f_max(fni) -> fmax;
    lconstant Cstring = 'Evaluate (APPLY #\'~3*~1{~S~} *TRACE-ARGS*)';
    if fmin and (nargs < fmin) then
        if fmax then (fmax - fmin) else 1 endif -> fmax;
        err_p(if continue then Cstring endif,
              '~S expects ~[~:;at least ~]~R argument~:P',
              [% f_name(fni), fmax, fmin %])
    elseif fmax and (nargs > fmax) then
        err_p(if continue then Cstring endif,
              '~S expects ~[~:;at most ~]~R argument~:P',
              [% f_name(fni), fmax - fmin, fmax %])
    endif
enddefine;


define lconstant Nargs_cerror(pdr);
    dlocal trace_args;
    cons_arglist() -> trace_args;
    Check_nargs(function_info(pdr), LISP_N_ARGS, program_cerror, true);
    dest_arglist(trace_args)
enddefine;


define runtime_nargs_error();
    lvars pdr;
    caller(1) -> pdr;
    Nargs_cerror(pdr);
    chainfrom(pdr, pdr)
enddefine;


define check_nargs() with_nargs 2;
    Check_nargs(program_error, false)
enddefine;


define check_fmin_apply(pdr, fmin);
    until LISP_N_ARGS == fmin do
        Nargs_cerror(pdr)
    enduntil;
    fast_chain(pdr)
enddefine;


/* Applying Common Lisp functions */

define funcall(fn, args);
    destlist(args) -> LISP_N_ARGS;
    fast_chain(checkr_function(fn))
enddefine;


define cl_apply(fn, arg, args);
    arg;
    destlist(args) -> arg;
    destlist() fi_+ arg -> LISP_N_ARGS;
    fast_chain(checkr_function(fn))
enddefine;


define global make_pop11_procedure(fn, nargs);
    if pop_true(nargs) then
        procedure() with_nargs 2;
            -> LISP_N_ARGS;
            fast_chain()
        endprocedure(% checkr_function(fn), check_positive(nargs) %)
    else
        procedure() with_nargs 1;
            stacklength() fi_- 1 -> LISP_N_ARGS;
            fast_chain()
        endprocedure(% checkr_function(fn) %)
    endif
enddefine;


/* COMPLEMENT */

define lisp_not_true_apply() with_nargs 1;
    if fast_apply() then nil else true endif
enddefine;


define complement_apply() with_nargs 1;
    if fast_apply() == nil then true else nil endif
enddefine;


define complement_function_info(fn) -> fni;
    lvars name;
    lconstant C = 'COMPLEMENT-';

    copy(function_info(fn)) -> fni;
    fni_name(fni) -> name;
    if issymbol(name) then
        C <> symbol_string(name)
    elseif name then
        C sys_>< name
    else
        genstring(C)
    endif -> fni_name(fni);
    false -> fni_file(fni)
enddefine;


define complement(fn);
    consclosure(complement_apply, checkr_function(fn), 1)
enddefine;


/* CONSTANTLY */

define always_function_info(val) -> fni;
    new_function_info(genstring('CONSTANTLY-')) -> fni;
    fill_function_info(fni, 0, false, 1)
enddefine;


define always(val) -> val;
    erasenum(LISP_N_ARGS)
enddefine;


define constantly(val);
    consclosure(always, val, 1)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  7 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Apr 26 1994
        checkr_function (and hence Lisp FUNCALL and APPLY) now handles
        function names.
--- John Williams, Mar 14 1994
        Reversed last change for backward compatability.
--- John Williams, Feb 23 1994
        checkr_function no longer accepts lambda expressions (Steele 1990 p145).
--- John Williams, Feb 14 1994
        Fixed compiled_function_p and functionp for Steele 1990 (p 38 & 103).
--- John Williams, Aug 27 1993
        Added stuff for complement function. Tidied up.
 */
