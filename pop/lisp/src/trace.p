/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lisp/src/trace.p
 > Purpose:         Tracing package for Common Lisp
 > Author:          John Williams, Oct 1 1986 (see revisions)
 > Documentation:   CLtL, p440-1
 > Related Files:   C.all/lisp/src/misc.lsp (for CL TRACE and UNTRACE)
 */

lisp_compile_mode;

section $-lisp;

vars
    trace_args          =   [],
    trace_results       =   [],
    tracelist           =   [],
    trace_print_length  =   5,
    trace_print_level   =   3,
    ;

propsheet_idents
    trace_print_length, trace_print_level;


/* Trace printing */

define lconstant Trace_pr(name, mode, args);
    dlocal print_length, print_level, standard_output = trace_output;
    RESET_PRINT_VARS;
    SET_CUCHAROUT;
    unless trace_print_length == @:IGNORE do
        trace_print_length -> print_length
    endunless;
    unless trace_print_level == @:IGNORE do
        trace_print_level -> print_level
    endunless;
    systrace_pr(name, mode == 0, args, false)
enddefine;


define vars lisptrace_proc(procedure pdr, name, upd, dobreak_ref);
    dlocal trace_args, trace_results;
    lconstant
        TRACE_ENTRY = conspair('Trace Entry', dup()),
        TRACE_EXIT = conspair('Trace Exit', dup()),
        ;

    cons_arglist() -> trace_args;
    isfunction_token(name) or name -> name;
    Trace_pr(name, 0, trace_args);
    if fast_cont(dobreak_ref) then
        @BREAK(TRACE_ENTRY, name, 2) ->
    endif;
    conslist
        (#| dest_arglist(trace_args);
            pdr();
        |#)
        -> trace_results;
    Trace_pr(name, 1, trace_results);
    if fast_cont(dobreak_ref) then
        @BREAK(TRACE_EXIT, name, 2)
    endif;
    destlist(trace_results) ->
enddefine;


/* TRACE and UNTRACE macros */

define lconstant Checkr_trace_ftok(fname);
    lvars sym, ftok;
    unless (fname_sym(fname, false) ->> sym) do
        warn('Ignoring invalid function name', [^fname]) ->;
        return(false)
    endunless;
    proclaim([^@NOTINLINE ^sym]);
    sf_token(sym) -> ftok;
    if ft_flags(ftok) == _FT_SPECIAL then
        warn('Cannot trace special-form', [^fname]) ->;
        return(false)
    endif;
    unless isboundtoken(ftok) do
        warn('Cannot trace unbound function', [^fname]) ->;
        return(false)
    endunless;
    ftok
enddefine;


define lconstant Checkr_trace_methods(mspec);
    /* mspec is list of the form (<gfn name> <qualifiers> <p-specs>) */
    lvars p_specs, quals, name, gfn, iom;

    go_on destlist(mspec) to ERR TWO THREE else ERR;
ERR:
    warn('Malformed method specifier in trace/untrace form', [^mspec]) ->;
    return(false);
THREE:
    -> p_specs;
TWO:
    -> quals -> name;

    if fboundp(name)
    and (is_gfn_clos(fdefinition(name)) ->> gfn) then
        if quals == @:ALL then
            maplist(gfn_methods(gfn), Return_iom)
        else
            lisp_apply(gfn_instance(gfn), quals, p_specs, nil,
                        fdefinition(@FIND-METHOD), 4, 1)
                -> iom;
            if iom == nil then
                warn('Cannot trace non-existent method', [^mspec]) ->;
                false
            else
                [^iom]
            endif
        endif
    else
        warn('~S is not a generic function', [^name ^mspec]) ->;
        false
    endif
enddefine;


define lconstant Set_break(pdr, dobreak);
    if pdpart(frozval(3, pdr)) == lisptrace_proc then
        dobreak -> cont(frozval(1, frozval(3, pdr)))
    endif
enddefine;


define lconstant Trace_pdr(pdr, name, dobreak);
    if istraced(pdr) then
        Set_break(pdr, dobreak);
        false
    else
        systrace(% pdr, name, lisptrace_proc(% consref(dobreak) %), false %)
    endif
enddefine;


define lconstant Untrace_pdr(pdr) -> p;
    if (istraced(pdr) ->> p) then
        Set_break(pdr, false)
    else
        false -> p
    endif
enddefine;


define lconstant Trace_assign(pdr, ftok);
    lvars prot;
    isprotected(ftok) -> prot;
    sysunprotect(ftok);
    pdr -> valof(ftok);                     ;;; N.B. not ft_valof
    if prot then sysprotect(ftok) endif
enddefine;


define lconstant Trace_ftok(ftok, dobreak);
    lvars pdr;
    if (Trace_pdr(valof(ftok), ftok, dobreak) ->> pdr) then
        Trace_assign(pdr, ftok);
        true
    else
        false
    endif
enddefine;


define lconstant Untrace_ftok(ftok);
    lvars pdr, p;
    if (Untrace_pdr(valof(ftok)) ->> pdr) then
        Trace_assign(pdr, ftok);
        if isref(ft_compiler_macro(ftok) ->> p) then
            cont(p) -> ft_compiler_macro(ftok)
        endif;
        true
    else
        false
    endif
enddefine;


define lconstant Trace_methods(iom_list, dobreak);
    lvars iom, method, pdr;
    [% for iom in iom_list do
        Accept_iom(iom) -> method;
        if (Trace_pdr(method_body(method), iom, dobreak) ->> pdr) then
            pdr -> method_body(method);
            iom
        endif
    endfor %]
enddefine;


define lconstant Untrace_methods(iom_list);
    lvars iom, method, pdr;
    [% for iom in iom_list do
        Accept_iom(iom) -> method;
        if (Untrace_pdr(method_body(method)) ->> pdr) then
            pdr -> method_body(method);
            iom
        endif
    endfor %]
enddefine;


define lisp_trace(args);
    lvars arg, fname, dobreak, iom_list, ftok;
    if args == [] then
        return(tracelist)
    endif;
    [% for arg in_cl_list args do
        if islistlength(arg, 3)
        and fast_subscrl(2, arg) == @:BREAK then
            fast_subscrl(1, arg), pop_true(fast_subscrl(3, arg))
        else
            arg, false
        endif -> (fname, dobreak);
        if fname starts_with @:METHOD then
            if (Checkr_trace_methods(back(fname)) ->> iom_list)
            and (Trace_methods(iom_list, dobreak) ->> iom_list) then
                conspair(fname, tracelist) -> tracelist;
                dl(iom_list)
            endif
        elseif (Checkr_trace_ftok(fname) ->> ftok) then
            if Trace_ftok(ftok, dobreak) then
                conspair(fname, tracelist) -> tracelist;
                fname
            endif
        endif
    endfor %]
enddefine;


define lisp_untrace(args);
    lvars fname, iom_list, iom, ftok;
    if args == [] then
        tracelist -> args
    endif;
    [% for fname in_cl_list args do
        if fname starts_with @:METHOD then
            if (Checkr_trace_methods(back(fname)) ->> iom_list)
            and (Untrace_methods(iom_list) ->> iom_list) then
                fast_ncdelete(fname, tracelist, nonop ==) -> tracelist;
                dl(iom_list)
            endif
        elseif (Checkr_trace_ftok(fname) ->> ftok) then
            if Untrace_ftok(ftok) then
                fast_ncdelete(fname, tracelist, equal) -> tracelist;
                fname
            endif
        endif
    endfor %]
enddefine;


[^lisptrace_proc ^systrace ^^db_hidden_functions] -> db_hidden_functions;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 15 1996
        Untrace_ftok now restores compiler macro if defined.
--- John Williams, Nov 30 1995
        sysPASSIGN will only preserve tracing when a function is redefined if
        the second frozval of lisptrace_proc is a function token (not symbol).
--- John Williams, Aug 11 1995
        Removed redundant lvar declarations.
--- John Williams, Jul 18 1995
        lisptrace_proc now passes caller info. to break.
--- John Williams, Feb 27 1995
        Changes for the Condition system (Steele 1990 ch 29).
--- John Williams, May 23 1994
        Added (trace (:method <name> :all)) option.
--- John Williams, Apr 26 1994
        TRACE and UNTRACE now cope with function names.
        Can trace and untrace methods with the syntax
            (trace (:method <name> <qualifiers> <p-specs> ))
--- John Williams, Jun 17 1993
        Implemented @:IGNORE options to trace_print_length and
        trace_print_level.
 */
