/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/declare.p
 > Purpose:         Declarations
 > Author:          John Williams, Feb 13 1987 (see revisions)
 > Documentation:   CLtL, Chapter 9, p153-162
 > Related Files:
 */

lisp_compile_mode;

section $-lisp;

constant procedure (Declare_type, lamlist_info);

fastprocs front, back, destpair;


/* Set up a table mapping declaration specifiers to procedures */

defprop sym_declare_pdr;


define lconstant Declare_undone(args, sym);
    if lisp_system_building then
        advise('Ignoring unimplemented declaration: ~S', [[^sym ^^args]])
    else
        warn('Ignoring unimplemented declaration', [[^sym ^^args]]) ->;
        /* Note: if user exits out of WARN to top-level, the following
                 assignment will not take place.
        */
        erase -> sym_declare_pdr(sym)
    endif
enddefine;


define do_declaration(dec);
    lvars sym, pdr;
    checkr_name_from_list(dec, @DECLARATION) -> sym;
    if (sym_declare_pdr(sym) ->> pdr) then
        pdr(back(dec))
    elseif is_basic_system_type(sym) then
        Declare_type(dec)
    else
        warn('Ignoring unrecognised declaration', [^dec]) ->;
        /* Note if user exits out of WARN to top-level, the following
            assignment will not take place.
        */
        erase -> sym_declare_pdr(sym)
    endif
enddefine;


define declare(decs);
    lvars dec;
    dlocal Proclaiming = false;
    for dec in_cl_list decs do
        do_declaration(dec)
    endfor
enddefine;


define proclaim() with_nargs 1;
    dlocal Proclaiming = true;
    do_declaration()
enddefine;


define Declare(forms, nresults);    /* The special form */
    if isref(nresults) then             /* i.e. top level form */
        declare(forms)
    else
        warn('Ignoring misplaced declarations', []) ->
    endif
enddefine;


define lconstant Check_proclaiming(sym);
    unless Proclaiming do
        program_error('The ~S declaration must be made via PROCLAIM', [^sym])
    endunless
enddefine;


define lconstant Declaration_format_error(type, args);
    warn('Ignoring malformed ~S declaration', [^type ^args]) ->;
    exitfrom(do_declaration)
enddefine;


/* Declaration procedures */

define lconstant Declare_special(args);
    lvars sym;
    for sym in_cl_list args do
        lispSPECIAL(sym)
    endfor
enddefine;


define lconstant Declare_constant(args);
    if islistlength(args, 2) then
        Check_proclaiming(@POPLOG:CONSTANT);
        lispCONSTANT(destlist(args) ->)
    else
        Declaration_format_error(@POPLOG:CONSTANT, args)
    endif
enddefine;


define lconstant Declare_inline(args, bool);
    lvars fname;
    for fname in_cl_list args do
        lispINLINE(fname_sym(fname), bool)
    endfor
enddefine;


define Declare_type(args);          /* not lconstant 'cos referred to above */
    lvars type, sym;
    if ispair(args) then
        destpair(args) -> (type, args);
        type_expand(type) -> type;
        for sym in_cl_list args do
            lispTYPE(sym, type)
        endfor
    else
        warn('Missing type specifier in (DECLARE (TYPE)) form', []) ->
    endif
enddefine;


define lconstant Declare_ftype(args);
    lvars fspec, fnames, fname, fmin, fmax, fresults,
            sym, fni, ftok;

    unless ispair(args) do
        Declaration_format_error(@FTYPE, args)
    endunless;
    destpair(args) -> (fspec, fnames);

    if fspec == @FUNCTION then
        for fname in_cl_list fnames do
            lispFDECLARE(fname_sym(fname), _FT_FUNCTION)
        endfor;
        return
    elseif fspec == @MACRO then
        for fname in_cl_list fnames do
            check_name(fname, @MACRO);
            lispFDECLARE(fname, _FT_MACRO)
        endfor;
        return
    else
        unless islistlength(fspec, 3)
        and front(fspec) == @FUNCTION do
            Declaration_format_error(@FTYPE, fspec)
        endunless
    endif;

    /* fspec is list of the form (FUNCTION <lambda-list> <results-spec>) */
    lblock;
        lvars results, lamlist, oargs, rest;
        destlist(fspec) -> (, lamlist, results, );
        lamlist_info(lamlist, true) -> (fmin, oargs, rest);
        if rest then
            false
        else
            fmin + oargs
        endif -> fmax;
        if results starts_with @VALUES then
            lamlist_info(back(results), true) -> (fresults, oargs, rest);
            if rest or oargs /== 0 then
                false -> fresults
            endif
        else
            1 -> fresults
        endif
    endlblock;

    for fname in_cl_list fnames do
        fname_sym(fname) -> sym;
        new_function_info(sym) -> fni;
        fill_function_info(fni, fmin, fmax, fresults);
        current_sf_token(sym) -> ftok;
        if isboundtoken(ftok) then
            merge_function_info(fni, function_info(ft_valof(ftok)))
        endif;
        fni -> ft_function_info(ftok)
    endfor
enddefine;


define lconstant Declare_optimize(args);
    lvars item;
    for item in_cl_list args do
        if islistlength(item, 2) then
            lispOPTIMISE(destlist(item) ->)
        elseif atom(item) then
            lispOPTIMISE(item, 3)
        else
            false
        endif;
        unless (/* done */) do
            Declaration_format_error(@OPTIMIZE, item)
        endunless
    endfor
enddefine;


define lconstant Declare_dynamic_extent(args);
    lvars item;
    if Proclaiming then
        program_error('DYNAMIC-EXTENT declarations cannot be made with PROCLAIM',
                    [% conspair(@DYNAMIC-EXTENT, args) %])
    else
        for item in_cl_list args do
            if issymbol(item) then
                lispDYNAMIC_EXTENT(sv_token(item))
            elseif item matches [^@FUNCTION =] then
                lispDYNAMIC_EXTENT(current_sf_token(cadr(item)))
            else
                Declaration_format_error(@DYNAMIC-EXTENT, item)
            endif
        endfor
    endif
enddefine;


define lconstant Declare_declaration(args);
    lvars item, p;
    Check_proclaiming(@DECLARATION);
    for item in_cl_list args do
        unless (sym_declare_pdr(item) ->> p)
        and not(p is_closure_of Declare_undone)
        then
            erase -> sym_declare_pdr(item)
        endunless
    endfor
enddefine;


/* Set up sym_declare_pdr property */

Declare_declaration         -> sym_declare_pdr(@DECLARATION);
Declare_dynamic_extent      -> sym_declare_pdr(@DYNAMIC-EXTENT);
Declare_ftype               -> sym_declare_pdr(@FTYPE);
Declare_inline(% nil %)     -> sym_declare_pdr(@NOTINLINE);
Declare_inline(% true %)    -> sym_declare_pdr(@INLINE);
Declare_optimize            -> sym_declare_pdr(@OPTIMIZE);
Declare_special             -> sym_declare_pdr(@SPECIAL);
Declare_type                -> sym_declare_pdr(@TYPE);

Declare_undone(% @IGNORABLE %)      -> sym_declare_pdr(@IGNORABLE);
Declare_undone(% @IGNORE %)         -> sym_declare_pdr(@IGNORE);

Declare_constant            -> sym_declare_pdr(@POPLOG:CONSTANT);
Declare_optimize            -> sym_declare_pdr(@POPLOG:OPTIMISE);


/* Special form LOCALLY */

define Locally(forms, nresults);
    INIT_DECLARE_VARS;
    parse_body(forms, false, true) -> (forms, , );
    Progn(forms, nresults)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, May  5 1995
        Declare_undone now uses advise if called while lisp_system_building.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Feb  2 1995
        Declare_declaration now silences undone declarations.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Jun  7 1994
        Changes for POPLOG package.
--- John Williams, Apr 26 1994
        Declare_inline now handles function names, and also no longer
        automatically declares updaters.
--- John Williams, Feb 24 1994
        LOCALLY now a special form (Steele 1990 p221).
--- John Williams, Nov 12 1993
        Now only prints an "Unrecognised declaration" warning once.
--- John Williams, Aug 27 1993
        Uses new in_cl_list syntax. Replaced WID with "ident ..".
--- John Williams, Mar  3 1992
        OPTIMISE declaration now recognises COMPILATION-SPEED (not yet used).
        See BR rogere.52 and BR isl-fr.4415
 */
