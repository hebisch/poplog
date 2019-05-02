/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/tokens.p
 > Purpose:         Common Lisp variables and function tokens
 > Author:          John Williams, March 1988 (see revisions)
 > Documentation:
 > Related Files:
 */

lisp_compile_mode;

section $-lisp;

fastprocs front, back, chain;

constant procedure setf_method;


define fname_sym(fname) -> sym;
    lvars err = true, p, item;
    unless fname do
        /* Optional extra arg false means don't signal error */
        false -> err;
        -> fname
    endunless;
    if issymbol(fname) then
        fname -> sym
    elseif fname starts_with @SETF
    and (back(fname) -> p,
            ispair(p) and back(p) == [] and issymbol(front(p) ->> p))
    then
        setf_method(p) -> item;
        unless (isfunction_token(item) ->> sym) do
            if err then
                lisp_error('~S names a setf expander function', [^p])
            endif
        endunless
    elseif err then
        sf_token(fname)     /* Signals an error */
    else
        false -> sym
    endif
enddefine;


/* Recognising declared tokens */

sysunprotect("sysdeclare");

constant
    global_var_type,
    pop11_declare_perm_id = sysdeclare,
    ;


define isfunction_type(type);
    recursive_front(type) -> type;
    type == @FUNCTION
    or type == @COMPILED-FUNCTION
    or type == @GENERIC-FUNCTION
    or type == @STANDARD-GENERIC-FUNCTION
enddefine;


define lconstant Declare_vtok(vtok, const);
    consident(
        if isfunction_type(global_var_type(vtok)) then "procedure" else 0 endif,
        const, "perm") -> identof(vtok)
enddefine;


define lconstant Declare_ftok(ftok, const);
    consident("procedure", const, "perm") -> identof(ftok)
enddefine;


define is_declared_token(tok);
    lvars sym, pkg;
    dlocal sysdeclare = pop11_declare_perm_id;
    if isdeclared(tok) then
        true
    else
        isfunction_token(tok) or tok -> sym;
        symbol_package(sym) -> pkg;
        if pkg == pop11_package then
            identof(sym_to_word(sym)) -> identof(tok);
            true
        elseif pkg == keyword_package
        and tok == sym then
            Declare_vtok(tok, true);
            tok -> valof(tok);
            sysprotect(tok);
            true
        else
            false
        endif
    endif
enddefine;


/* Recognising unbound tokens */

define isboundtoken(tok);
    is_declared_token(tok) and not(isundef(valof(tok)))
enddefine;


define boundp() with_nargs 1;
    isboundtoken(sv_token())
enddefine;


define fboundp() with_nargs 1;
    isboundtoken(sf_token(fname_sym()))
enddefine;


/* Declaring tokens */

vars
    constant_functions          =   true,       /* while Lisp is built */
    procedure lispprwarning     =   erase,
    ;


define lconstant Assign_undef_clos(tok);
    lvars p;
    exec_non_func(% consundef(tok) %) ->> p -> fast_idval(identof(tok));
    set_undef_clos(p);
    isfunction_token(tok) or tok -> pdprops(p)      /* for f_name */
enddefine;


define lconstant Init_vtok(vtok);
    if identtype(vtok) == "procedure" then
        Assign_undef_clos(vtok)
    else
        consundef(vtok) -> fast_idval(identof(vtok))
    endif
enddefine;


define lconstant Init_ftok(ftok);
    ft_perm_flags(ftok) _biset _FT_FUNCTION -> ft_flags(ftok);
    Assign_undef_clos(ftok)
enddefine;


define vars sysdeclare(tok);
    if issymbol(tok) then
        unless is_declared_token(tok) do
            Declare_vtok(tok, false);
            Init_vtok(tok);
            lispprwarning(tok)
        endunless
    elseif isfunction_token(tok) then
        unless is_declared_token(tok) do
            Declare_ftok(tok, false);
            Init_ftok(tok);
            lispprwarning(tok)
        endunless
    else
        pop11_declare_perm_id(tok)
    endif
enddefine;


define re_declare_token(tok, const, init);
    lvars sym, idprops, val;
    dlocal pop_vm_flags = (pop_vm_flags _biset VM_NOPROT_PVARS);

    if is_declared_token(tok) then
        if (isfunction_token(tok) ->> sym) then
            if isprotected(tok) then
                redefine_cerror(sym, @FUNCTION)
            endif;
            "procedure" -> idprops
        else
            tok -> sym;
            unless isassignable(tok) do
                redefine_cerror(sym, @POPLOG:CONSTANT)
            endunless;
            0 -> idprops;
            if isfunction_type(global_var_type(tok)) then
                "procedure" -> idprops;
                unless identtype(tok) == "procedure" do
                    valof(tok) -> val;
                    if isundef(val) then
                        Assign_undef_clos(tok)
                    elseunless functionp(val) do
                        false -> global_var_type(tok);
                        lisp_cerror(
                            'Ignore FUNCTION declaration for ~S',
                            'Variable ~S already has non-functional value',
                            [^sym ^val]);
                        0 -> idprops
                    endif
                endunless
            endif
        endif;
        if isactive(tok) then
            conspair(idprops, isactive(tok)) -> idprops
        endif;
        ident_declare(tok, idprops, const)
    elseif isfunction_token(tok) then
        Declare_ftok(tok, const);
        if init then Init_ftok(tok) endif
    else
        Declare_vtok(tok, const);
        if init then Init_vtok(tok) endif
    endif
enddefine;


/* Making tokens unbound */

define makunbound(sym) -> sym;
    lvars vtok;
    sv_token(sym) -> vtok;
    if isboundtoken(vtok) then
        sysunprotect(vtok);
        Init_vtok(vtok)
    endif
enddefine;


define fmakunbound(fname) -> fname;
    lvars ftok;
    sf_token(fname_sym(fname)) -> ftok;
    if isboundtoken(ftok) then
        sysunprotect(ftok);
        Init_ftok(ftok)
    endif
enddefine;


/* Accessing/updating tokens  */

define token_valof(tok) -> val;
    valof(tok) -> val;
    if isundef(val) then
        if (isfunction_token(tok) ->> val) then
            lisp_cerror('Try taking functional value of ~*~S again',
                        @UNDEFINED-FUNCTION,
                        {^@:NAME ^val ^@:OPERATION ^@:EVALUATE})
        else
            lisp_cerror('Try evaluating ~*~S again',
                        @UNBOUND-VARIABLE,
                        {^@:NAME ^tok ^@:OPERATION ^@:EVALUATE})
        endif;
        chain(tok, token_valof)
    endif
enddefine;


define symbol_value() with_nargs 1;
    token_valof(sv_token())
enddefine;


define updaterof symbol_value(val, sym);
    lvars vtok;
    sv_token(sym) -> vtok;
    re_declare_token(vtok, false, false);
    val -> valof(vtok)
enddefine;


define symbol_function() with_nargs 1;
    token_valof(sf_token())
enddefine;


define fdefinition() with_nargs 1;
    /* should really signal an error if arg is a symbol and not fboundp,
        but this would mean changing defstruct.p
    */
    valof(sf_token(fname_sym()))
enddefine;


define is_macro(sym);
    lvars ftok;
    current_sf_token(sym) -> ftok;
    if ft_flags(ftok) _bitst _FT_MACRO then
        valof(ftok)
    else
        false
    endif
enddefine;


define macro_function() with_nargs 2;
    dlocal Local_function_tokens;
    checkr_lexical_env(/* env */) -> Local_function_tokens;
    lisp_true(is_macro(/* sym */))
enddefine;


define special_form_p(sym);
    lvars ftok;
    current_sf_token(sym) -> ftok;
    if ft_flags(ftok) == _FT_SPECIAL then
        f_inline(valof(ftok))
    else
        false
    endif
enddefine;


define lconstant New_symbol_function(pdr, sym, flags);
    lvars ftok;
    sf_token(sym) -> ftok;
    checkr_function(pdr) -> pdr;
    re_declare_token(ftok, pop_true(constant_functions), false);
    ft_perm_flags(ftok) _biset flags -> ft_flags(ftok);
    unless issymbol(f_name(pdr)) do
        ft_name(ftok) -> f_name(pdr)
    endunless;
    /* Ensure that the sysPASSIGN called in ft_valof always executes */
    sysCOMPILE(pdr, ftok, updater(ft_valof))
enddefine;


define updaterof symbol_function() with_nargs 2;
    New_symbol_function(_FT_FUNCTION)
enddefine;


define updaterof fdefinition(pdr, fname);
    New_symbol_function(pdr, fname_sym(fname), _FT_FUNCTION)
enddefine;


define updaterof macro_function(pdr, sym, env);
    if pop_true(env) then
        lisp_error(
            'Cannot specify lexical environment when setting MACRO-FUNCTION',
            [^sym ^env])
    endif;
    New_symbol_function(pdr, sym, _FT_MACRO)
enddefine;


define updaterof special_form_p(pdr, sym);
    lvars dummy;
    if pop_true(pdr) then
        exec_special_form(% sym %) -> dummy;
        sym -> pdprops(dummy);
        New_symbol_function(dummy, sym, _FT_SPECIAL);
        pdr -> f_inline(dummy)
    else
        fmakunbound(sym) ->
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Apr 26 1994
        New procedure fname_sym for handling function names of the form
        (SETF fname). fboundp, fmakunbound, and fdefinition now all use
        fname_sym.
--- John Williams, Jul 12 1993
        Uses pop_true(constant_functions) instead of popdefineconstant
 */
