/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/multi.p
 > Purpose:         Constructs for receiving and passing multiple values
 > Author:          John Williams, Nov 27 1985 (see revisions)
 > Documentation:   Common Lisp Manual, p133-139
 */

lisp_compile_mode;

section $-lisp;

fastprocs front, back, destpair, for;


define lconstant Prog_multi(forms);
    INIT_STK_VARS;
    until endp(forms) do
        compile_form(destpair(forms) -> forms, false)
    enduntil
enddefine;


define Prog0(forms);
    INIT_STK_VARS;
    lispSAVE_STKLEN(false);
    until endp(forms) do
        compile_form(destpair(forms) -> forms, 0)
    enduntil
enddefine;


define Multiple_value_call(args, nresults);
    lvars fn, CALL;
    dlocal Tmp_ftok_list;
    unless ispair(args) do
        program_error('MULTIPLE-VALUE-CALL expects at least one argument', [])
    endunless;
    destpair(args) -> (fn, args);
    compile_fn(fn) -> fn -> CALL;
    procedure();
        INIT_STK_VARS;
        lispSAVE_STKLEN(false);
        lispSET_STKLEN(nresults, f_results(fn)) -> nresults;
        Prog_multi(args);
        sysCALLQ(stacklength);
        sysPUSH(Stklenvar);
        sysCALLQ(nonop fi_-);
        CALL(fn, false, nresults)
    endprocedure()
enddefine;


define Multiple_value_prog1(forms, nresults);
    unless ispair(forms) do
        program_error('MULTIPLE-VALUE-PROG1 expects at least one argument', [])
    endunless;
    compile_form(destpair(forms) -> forms, nresults);
    Prog0(forms)
enddefine;


/* Some inline versions of macros */

define Multiple_value_list(forms, nresults);
    lispSET_STKLEN(nresults, 1) -> nresults;
    sysPUSHQ(popstackmark);
    Prog_multi(forms);
    sysCALLQ(sysconslist);
    lispRESET_STKLEN(nresults)
enddefine;


define lconstant Parse_mv_form(name, forms) -> (varlist, nvars, form, forms);
    lvars sym;
    unless ispair(forms)
    and listp(destpair(forms) -> forms ->> form) do
        program_error('Missing or strange variable list in ~S form', [^name])
    endunless;
    0 -> nvars;
    [] -> varlist;
    for sym in_cl_list form do
        conspair(sym, varlist) -> varlist;      ;;; varlist is reversed!
        nvars fi_+ 1 -> nvars
    endfor;
    unless ispair(forms) do
        program_error('Missing <values> form in ~S form', [^name])
    endunless;
    destpair(forms) -> (form, forms)
enddefine;


define Multiple_value_bind(forms, nresults);
    lvars varlist, nvars, form;
    Parse_mv_form(@MULTIPLE-VALUE-BIND, forms)
        -> (varlist, nvars, form, forms);
    compile_form(form, nvars);
    compile_let(forms, nresults, varlist, false)
enddefine;


define Multiple_value_setq(forms, nresults);
    lvars varlist, nvars, form, sym;

    /* N.B. the compiler macro for MULTIPLE-VALUE-SETQ will only call
        this procedure if none of the variables are symbol macros.
    */

    Parse_mv_form(@MULTIPLE-VALUE-SETQ, forms)
        -> (varlist, nvars, form, forms);

    unless forms == [] do
        program_error('Excess arguments in MULTIPLE-VALUE-SETQ form', [^forms])
    endunless;

    compile_form(form, nvars);
    @NIL -> sym;
    for sym in varlist do
        lispPOP(sym)
    endfor;
    lispPUSHN(sym, nresults)
enddefine;


define Nth_value(forms, nresults);
    lvars n, tmp;
    dlocal Tmp_vtok_list;

    unless islistlength(forms, 2)
    and (destlist(forms) -> (n, forms, ),
         isinteger(n))
    then
        program_error('Malformed SYS:%NTH-VALUE form', [])
    endunless;

    if n == 0
    and (deref(nresults) -> tmp, tmp == 1 or not(isinteger(tmp))) then
        compile_form(forms, 1)
    else
        lispNEW_VTOK() -> tmp;
        check_positive(n) ->;
        compile_form(forms, n fi_+ 1);
        lispPOP(tmp);
        lispRESET_STKLEN(n);            /* i.e. erasenum(n) */
        lispPUSHN(tmp, nresults)
    endif
enddefine;


define nth_value(N) -> n;
    /* top item on the stack is the (0-origin) index of the desired value */
    /* N is the actual number of values on the stack, plus 1 (for the index) */
    check_positive() fi_+ 1 -> n;               /* n = 1-origin stack index */
    if n fi_< N then
        subscr_stack(N fi_- n)
    else
        nil
    endif -> n;
    erasenum(N fi_- 1)
enddefine;


define values_list(list);
    until endp(list) do destpair(list) -> list enduntil
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
        Changes for symbol macros. Also support for NTH-VALUE.
 */
