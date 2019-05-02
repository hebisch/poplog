/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/setf.p
 > Purpose:         Computing SETF expansions
 > Author:          John Williams, Dec 10 1985 (see revisions)
 > Documentation:   CLtL, ch 7.2
 > Related Files:   C.all/lisp/src/setf.lsp
 */

lisp_compile_mode;

section $-lisp;

fastprocs front, back, destpair, ncrev, chain;


define lconstant Create_updater_caller(usym, ftok);
    lvars p, u, ufni;
    dlocal constant_functions;
    if isboundtoken(ftok)
    and isprocedure(ft_valof(ftok) ->> p)
    and (updater(p) ->> u)
    and not(isinheap(p)) then
        true -> constant_functions
    else
        sysPROCEDURE(false, 1);
        sysUCALL(ftok);
        sysENDPROCEDURE() -> u
    endif;
    new_function_info(usym) ->> ufni -> function_info(u);
    fill_function_info(ufni, 1, false, 0);
    u -> symbol_function(usym)
enddefine;


define setf_method(sym);
    lvars create = true, ftok, item, usym;
    if isboolean(sym) then
        sym -> create;
        -> sym
    endif;
    current_sf_token(sym) -> ftok;
    if (ft_setf_method(ftok) ->> item) then
        item
    elseif create then
        make_symbol(genstring('SETF-' <> symbol_string(sym))) -> usym;
        sf_token(usym) -> ft_setf_method(ftok);
        if symbol_package(sym) == pop11_package then
            Create_updater_caller(usym, ftok)
        endif;
        sf_token(usym)
    else
        false
    endif
enddefine;


define updaterof setf_method(item, sym);
    lvars ftok;
    current_sf_token(sym) -> ftok;
    if isprotected(ftok)
    and not(lisp_system_building) then
        redefine_cerror(sym, @SETF)
    endif;
    item -> ft_setf_method(ftok)
enddefine;


define get_setf_method(form, env, multi);
    lvars argvars, argvals, store, storeform, temp, args, name;
    dlocal Local_function_tokens = checkr_lexical_env(env);

    [] ->> argvars -> argvals;
    false ->> store -> storeform;

    repeat
        quitif(atom(form));
        setf_method(front(form), false) -> storeform;
        quitif(storeform);
        macroexpand1(form, false) -> (form, temp);
        quitif(temp == nil)
    endrepeat;

    if atom(form) then
        check_name(form, @VARIABLE);
        if (symbol_macro(form) ->> temp) then
            chain(temp, Local_function_tokens, multi, get_setf_method)
        endif;
        gensymbol('S') -> store;
        [^@SETQ ^form ^store] -> storeform;
        [^store] -> store
    elseif storeform starts_with @DEFINE-SETF-METHOD then
        lisp_apply(form, Local_function_tokens, back(storeform), 2, 5)
            -> (argvars, argvals, store, storeform, form);
        unless multi or islistlength(store, 1) do
            lisp_error('Setf expander doesn\'t return 1 store variable',
                        [^store])
        endunless
    else
        destpair(form) -> (name, args);
        [% for form in_cl_list args do
            if is_constant_value(form) then
                form
            else
                conspair(form, argvals) -> argvals;
                conspair(gensymbol('V'), argvars) -> argvars;
                front(argvars)
            endif
        endfor %] -> args;
        ncrev(argvals) -> argvals;
        ncrev(argvars) -> argvars;

        gensymbol('S') -> store;
        if storeform starts_with @DEFSETF then
            lisp_apply(
                store, destlist(args) -> temp, back(storeform), temp + 1, 1)
        elseif issymbol(storeform) then
            [^storeform ^^args ^store]
        elseif (isfunction_token(storeform) ->> temp) then
            [^@PROGN [^temp ^store ^^args] ^store]
        elseif storeform then
            lisp_error('System error: strange result from setf_method',
                        [^name ^storeform])
        else
            [^@PROGN
                [^@FUNCALL [^@FUNCTION [^@SETF ^name]] ^store ^^args]
                ^store]
        endif -> storeform;

        [^store] -> store;
        [^name ^^args] -> form
    endif;

    argvars;    ;;; temp vars
    argvals;    ;;; subforms to bind to above
    store;      ;;; store variables
    storeform;  ;;; storing form
    form;       ;;; access form
enddefine;


/* This function is used by the macro setf */

define setf_is_updater(place);
    lvars usym;
    if ispair(place)
    and (isfunction_token(setf_method(front(place), false)) ->> usym)
    and symbol_package(usym) == system_package then
        usym
    else
        false
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Jun 30 1995
        Added setf_is_updater.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Jan 18 1995
        Now explicitly generates code to return value of store variable
        if no SETF method is defined (so that forward references to
        SETF methods for DEFSTRUCT slots work correctly).
--- John Williams, Nov 10 1994
        Bug fix: lvar create in setf_method now always initialised to true.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Apr 26 1994
        Changes for function names, symbol macros, and forward references
        to SETF methods.
--- John Williams, Jan 18 1994
        No longer needs to explicitly increment gensym_count.
--- John Williams, Dec  1 1993
        Number of args checking in complex defsetf improved.
--- John Williams, Aug 11 1993
        Fixed for Steele 1990 gensymbol.
--- John Williams, Jul 12 1993
        Uses constant_functions instead of popdefineconstant
--- John Williams, Mar  4 1992
        Improved the error message when -get_setf_method- is applied to
        an undefined function (cf BR rogere.54)
--- John Williams, Mar  4 1992
        -get_setf_method- now only expans macros until a setf method is found
 */
