/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/let.p
 > Purpose:         LET, LET*, SYMBOL-MACROLET
 > Author:          John Williams, Nov  7 1985 (see revisions)
 > Documentation:   CLtL, section 7.5
 */

lisp_compile_mode;

section $-lisp;

fastprocs front, back, destpair, for;


define lconstant Parse_let_form(name, forms) -> (varlist, values, forms);
    lvars list, form, sym, val;
    unless ispair(forms)
    and (destpair(forms) -> (list, forms), listp(list)) do
        program_error('Missing or strange variable list in ~S form', [^name])
    endunless;
    [] -> varlist;
    [% for form in_cl_list list do
        if issymbol(form) and form /== nil then
            form, nil
        elseif islistlength(form, 1) then
            front(form), nil
        elseif islistlength(form, 2) then
            front(form), front(back(form))
        else
            program_error('Malformed (sym value) binding in ~S form', [^name])
        endif -> (sym, val);
        conspair(sym, varlist) -> varlist;      ;;; N.B. varlist is reversed
        val;
    endfor %] -> values
enddefine;


define parse_declarations(decs) -> (specials, functions, debug, decs);
    lvars specials = [], functions = Function_vars, debug = Debug,
            form, dec, sym;
    [% for form in decs do
        for dec in cdr(form) do             ;;; cdr to skip DECLARE
            dec;
            checkr_name_from_list(dec, @DECLARATION) -> sym;
            back(dec) -> dec;
            if sym == @TYPE then
                ;;; car in case dec is empty
                car(dec) -> sym
            endif;
            if sym == @SPECIAL then
                append(specials, dec) -> specials
            elseif isfunction_type(sym) then
                /* Note: the type associated with sym in Local_var_types
                    may be AND'ed with a previous type of sym. However,
                    for the purposes of lispLOCAL all we need to know is
                    that the type of sym is some sub-type of FUNCTION.
                */
                append(functions, dec) -> functions
            elseif sym == @OPTIMIZE
            or sym == @OPTIMISE then
                if fast_lmember(@DEBUG, dec)
                or lmember_=([^@DEBUG 3], dec) then
                    3 -> debug
                endif
            endif
        endfor
    endfor %] -> decs
enddefine;


define compile_let(forms, nresults, varlist, values);
    lvars decs, sym, item, has_specials = false;
    INIT_DECLARE_VARS;
    dlocal BoundSpecials = false, Symbol_macros;

    parse_body(forms, false, false) -> (forms, decs, );
    parse_declarations(decs) -> (Specials, Function_vars, Debug, decs);

    /* See if there are special bindings */

    define lconstant Is_special(sym);
        lmember(sym, Specials) or is_global_special(sv_token(sym))
    enddefine;

    for sym in varlist do
        if ispair(sym) then
            for item in_cl_list sym do
                if Is_special(item) then
                    true -> has_specials;
                    quitloop(2)
                endif
            endfor
        elseif Is_special(sym) then
            true -> has_specials;
            quitloop
        endif
    endfor;

    /* Now generate the code */

    lispLET(has_specials) -> has_specials;
    if values then
        /* Values haven't already been evaluated - must be LET* */
        destlist(varlist) ->;
        for item in values do
            -> sym;
            if ispair(sym) then
                compile_form(item, listlength(sym));
                repeat destlist(sym) times lispLOCAL_POP() endrepeat
            else
                compile_form(item, 1);
                lispLOCAL_POP(sym)
            endif;
        endfor
    else
        /* Values already on stack - from LET or MULTIPLE-VALUE-BIND */
        applist(varlist, lispLOCAL_POP)
    endif;
    applist(decs, do_declaration);
    Progn(forms, nresults);
    lispENDLET(has_specials)
enddefine;


define Let(forms, nresults);
    lvars varlist, values;
    Parse_let_form(@LET, forms) -> (varlist, values, forms);
    compile_args(values) ->;
    compile_let(forms, nresults, varlist, false)
enddefine;


define Let_*(forms, nresults);
    lvars varlist, values;
    Parse_let_form(@LET*, forms) -> (varlist, values, forms);
    compile_let(forms, nresults, varlist, values)
enddefine;


define Symbol_macrolet(forms, nresults);
    lvars syms, values, val;
    INIT_DECLARE_VARS;
    dlocal Symbol_macros = Symbol_macros or [];
    Parse_let_form(@SYMBOL-MACROLET, forms) -> (syms, values, forms);
    repeat destlist(values) times
        -> val;
        acons(destpair(syms) -> syms, val, Symbol_macros)
            -> Symbol_macros
    endrepeat;
    parse_body(forms, false, true) -> -> -> forms;
    Progn(forms, nresults)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Oct  7 1994
        Bug fix to parse_declarations.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Apr 26 1994
        Added SYMBOL-MACROLET. Removed COMPILER-LET.
--- John Williams, Aug 31 1993
        Let and Let_* upgraded to Steele 1990. Tidied up.
 */
