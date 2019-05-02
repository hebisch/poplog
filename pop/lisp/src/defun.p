/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/defun.p
 > Purpose:         Defining Common Lisp macros and functions
 > Author:          John Williams, May 29 1987 (see revisions)
 > Documentation:   CLtL, p59-67
 > Related Files:   C.all/lisp/src/defs.lsp
 */

lisp_compile_mode;

section $-lisp;

fastprocs front, back, destpair, for, idval;

defprop f_lamex;


define lconstant Is_dotted_pair(item);
    ispair(item) and atom(back(item) ->> item) and item /== []
enddefine;


define parse_definition(def, type) -> (name, lamlist, body, type);
    unless ispair(def) do
        program_error('Missing name in ~(~A~) definition',
                    [^(type or @FUNCTION)])
    endunless;
    destpair(def) -> (name, def);
    if Is_dotted_pair(name) then
        if type and type /== back(name) then
            program_error('Inconsistent function type & name', [^type ^name])
        endif;
        destpair(name) -> (name, type)
    elseunless type then
        @FUNCTION -> type
    endif;
    unless ispair(def)
    and (destpair(def) -> (lamlist, body); listp(lamlist)) do
        program_error('Missing lambda-list in ~(~A~) definition', [^type])
    endunless
enddefine;


define compile_function() -> pdr with_nargs 1;
    lvars name, type, doc;
    dlvars lamlist, body, decs, decs_todo, blockname;
    INIT_DECLARE_VARS;

    parse_definition(false) -> (name, lamlist, body, type);
    parse_body(body, true, false) -> (body, decs, doc);
    parse_declarations(decs) -> (Specials, Function_vars, Debug, decs_todo);

    name -> blockname;
    if type == @FUNCTION
    and name starts_with @SETF then
        fname_sym(name);
        cadr(name) -> blockname;
        -> name
    elseif name then
        check_name(name, type)
    endif;

    lispFCOMPILE(
        name,
        type,
        procedure();
            lvars id = consident(0, false, "lex");
            [] -> idval(id);
            compile_lamlist(lamlist);
            applist(decs_todo, do_declaration);
            compile_body(blockname, body, id);
            pop_true(idval(id)) -> f_results(Current_function_info)
        endprocedure) -> pdr;

    if doc then
        if type == @MACRO then @FUNCTION -> type endif;
        lisp_apply(
            doc,
            name or pdr,
            if issymbol(name) then type endif,
            ft_valof(setf_method(@DOCUMENTATION)),
            if issymbol(name) then 3 else 2 endif,
            0)
    endif;

    if Debugging then
        /* Save lambda expression */
        if blockname then
             [[^@BLOCK ^blockname ^^body]] -> body
        endif;
        [^@LAMBDA ^lamlist ^^decs ^^body];
        if is_protected_closure(pdr) then
            sysPUSHQ();
            sysPUSHQ_DISCOUNT_LEX(pdr);
            sysUCALLQ(f_lamex)
        else
            -> f_lamex(pdr)
        endif
    endif
enddefine;


define compile_macro() with_nargs 1;
    lvars name, lamlist, body, decs, doc;

    parse_definition(@MACRO) -> (name, lamlist, body, );
    parse_body(body, true, false) -> (body, decs, doc);

    lconstant Callform = @#:CALLFORM, Env = @#:ENV;

    compile_function(
        [^(conspair(name, @MACRO)) [^Callform ^Env]
            ^(if doc then doc endif)
            ^(make_destructuring_form(
                Callform, Env, nil, name, lamlist, decs, body))])
enddefine;


define compile_lambda(def);
    compile_function(conspair(false, back(def)))
enddefine;


define function_lambda_expression(p);
    lisp_true(f_lamex(p));
    lisp_true(is_lexical_closure(p));
    lisp_true(f_name(p))
enddefine;


/* FLET, LABELS and MACROLET */

define lconstant Parse_flet_form(forms, type) -> (names, defs, forms);
    lvars def, name, sym;
    if atom(forms)
    or not(listp(front(forms))) then
        program_error('Missing ~(~A~) definitions', [^type])
    else
        destpair(forms) -> (defs, forms);
        [% for def in_cl_list defs do
            car(def) -> name;
            if type == @FUNCTION then
                fname_sym(name)
            else
                name
            endif -> sym;
            if on_stack(sym) then
                program_error('Duplicate local ~(~A~) name ~S', [^type ^name])
            else
                sym
            endif
        endfor %] -> names
    endif
enddefine;


define Flet(forms, nresults);
    lvars names, defs, name, pdr;
    dlocal Local_function_tokens;
    Parse_flet_form(forms, @FUNCTION) -> (names, defs, forms);
    maplist(defs, compile_function) -> defs;
    for name, pdr in names, defs do
        lispFDECLARE(name, _FT_FUNCTION _biset _FT_LOCAL);
        lispFASSIGN(pdr, name)
    endfor;
    ;;; sys_grbg_list(names);
    ;;; sys_grbg_list(defs);
    Locally(forms, nresults)
enddefine;


define Macrolet(forms, nresults);
    lvars names, defs, name, pdr;
    dlocal Local_function_tokens;
    Parse_flet_form(forms, @MACRO) -> (names, defs, forms);
    sysCOMPILE(defs, compile_macro, maplist) -> defs;
    for name, pdr in names, defs do
        lispFDECLARE(name, _FT_MACRO _biset _FT_LOCAL);
        lispFASSIGN(pdr, name)
    endfor;
    ;;; sys_grbg_list(names);
    ;;; sys_grbg_list(defs);
    Locally(forms, nresults)
enddefine;


define Labels(forms, nresults);
    lvars names, defs, forms, name, decs, dec, def;
    dlocal Local_function_tokens;

    Parse_flet_form(forms, @FUNCTION) -> (names, defs, forms);

    for name in names do
        lispFDECLARE(name, _FT_FUNCTION _biset _FT_LOCAL)
    endfor;

    parse_body(forms, false, false) -> (forms, decs, );

    ;;; here should process inline/notinline/ftype declarations
    ;;; referring to any of the local function names.
    ;;; however, inline has no meaning for local functions in Poplog
    ;;; so only need to check for ftype:

    lblock;
        lvars form, dec, fspec, d;
        for form in decs do
            [% for dec in cdr(form) do
                if dec starts_with @FTYPE then
                    cadr(dec) -> fspec;
                    for name in_cl_list cddr(dec) do
                        [^@FTYPE ^fspec ^name] -> d;
                        if lmember_=(name, names) then
                            do_declaration(d)
                        else
                            d
                        endif
                    endfor
                else
                    dec
                endif
            endfor %] -> decs
        endfor
    endlblock;

    for name, def in names, defs do
        lispFASSIGN(compile_function(def), name)
    endfor;
    ;;; sys_grbg_list(names);

    applist(decs, do_declaration);
    Progn(forms, nresults)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Jun  6 1995
        Added f_lamex and function_lambda_expression.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Feb 24 1995
        Fixed bug in compile_function (it was always using the same ident
        for recording numbers of results produced by the function).
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Apr 26 1994
        Functions defined with LAMBDA can now also have documentation
        strings, attached to the procedure record rather then the name.
        FLET and LABELS now allow declarations in the main body of
        the code (Steele 2 p155).
 */
