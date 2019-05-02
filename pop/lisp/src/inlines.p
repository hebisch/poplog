/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lisp/src/inlines.p
 > Purpose:         Inline code planting versions of some functions
 > Author:          John Williams, Jul 22 1987 (see revisions)
 > Documentation:
 > Related Files:   C.all/lisp/src/exporti.p, C.all/lisp/src/cmacros.lsp
 */

lisp_compile_mode;

section $-lisp;

fastprocs front, back, destpair, repeat, frozval;

lconstant macro (
    ABORT_CALLI = [ false -> Optimised; exitto(lispCALLI); ],
    );


define lconstant Check_nargs(args, nargs, test_p);
    unless test_p(args, nargs) do
        ABORT_CALLI
    endunless
enddefine;


vars inline_reduce_limit = 5;

define lconstant Check_space(nargs);
    if ispair(nargs) then
        listlength(nargs) -> nargs
    endif;
    if nargs > inline_reduce_limit
    or (Space == 3 and nargs > 2) then
        ABORT_CALLI
    endif
enddefine;


define lconstant Inline_reduce(args, nresults, pdr, init);
    Check_space(args);
    lispSET_STKLEN(nresults, 1) -> nresults;
    if endp(args) then
        lispPUSHQ(init)
    else
        repeat (compile_args(args) - 1) times
            sysCALLQ(pdr)
        endrepeat
    endif;
    lispRESET_STKLEN(nresults)
enddefine;


define lconstant Inline_any(args, nresults, pdr, nargs);
    Check_nargs(args, nargs, islistlength);
    lispSET_STKLEN(nresults, 1) -> nresults;
    compile_args(args) ->;
    if ispair(pdr) then
        sysCALLQ(front(pdr));
        sysCALLQ(back(pdr))
    else
        sysCALLQ(pdr)
    endif;
    lispRESET_STKLEN(nresults)
enddefine;


define updaterof lconstant Inline_any(args, nresults, pdr, nargs);
    Check_nargs(args, nargs + 1, islistlength);
    lispSET_STKLEN(nresults, 0) -> nresults;
    compile_args(args) ->;
    if ispair(pdr) then
        sysCALLQ(front(pdr));
        sysUCALLQ(back(pdr))
    else
        sysUCALLQ(pdr)
    endif;
    lispRESET_STKLEN(nresults)
enddefine;


/* Arithmetic */

define lconstant Inline_-(args, nresults, op, inv_op, unary_op);
    lvars n;
    listlength(args) -> n;
    Check_nargs(n, 1, nonop >=);
    Check_space(n);
    lispSET_STKLEN(nresults, 1) -> nresults;
    if n == 1 then
        front(args) -> n;
        if isnumber(n) then
            lispPUSHQ(unary_op(n))
        else
            compile_form(n, 1);
            sysCALLQ(unary_op)
        endif
    else
        repeat (compile_args(args) - 2) times
            sysCALLQ(inv_op)
        endrepeat;
        sysCALLQ(op)
    endif;
    lispRESET_STKLEN(nresults)
enddefine;


define lconstant Inline_num_compare(args, nresults, test);
    lvars n, endlab, lab;
    dlocal Stackoffset;
    listlength(args) -> n;
    unless n == 2 or n == 3 do
        ABORT_CALLI
    endunless;
    lispSET_STKLEN(nresults, 1) -> nresults;
    compile_arg(destpair(args) -> args);
    compile_arg(destpair(args) -> args);
    sysNEW_LABEL() -> endlab;
    if n == 3 then
        sysPUSHS(1);
        Stackoffset + 1 -> Stackoffset;
        compile_arg(destpair(args) -> args);
        sysCALLQ(test);
        sysIFSO(sysNEW_LABEL() ->> lab);
        sysERASE(1);
        sysERASE(1);
        lispPUSHQ(nil);
        sysGOTO(endlab);
        sysLABEL(lab)
    endif;
    sysCALLQ(test);
    lispTRUE();
    sysLABEL(endlab);
    lispRESET_STKLEN(nresults)
enddefine;


define lconstant Choose_=();
    if Fast then sys_= else num_= endif
enddefine;


define inline_=(args, nresults);
    Inline_num_compare(args, nresults, Choose_=())
enddefine;


define inline_/=(args, nresults);
    Check_nargs(args, 2, islistlength);
    lispSET_STKLEN(nresults, 1) -> nresults;
    compile_args(args) ->;
    sysCALLQ(Choose_=());
    sysCALLQ(not);
    lispTRUE();
    lispRESET_STKLEN(nresults);
enddefine;


constant procedure (
    inline_*        =   Inline_reduce(% nonop *, 1 %),
    inline_+        =   Inline_reduce(% nonop +, 0 %),
    inline_-        =   Inline_-(% nonop -, nonop +, negate %),
    inline_/        =   Inline_-(% nonop /, nonop *, reciprocal %),
    inline_<        =   Inline_num_compare(% nonop < %),
    inline_<=       =   Inline_num_compare(% nonop <=%),
    inline_>        =   Inline_num_compare(% nonop > %),
    inline_>=       =   Inline_num_compare(% nonop >=%),
    inline_logand   =   Inline_reduce(% boole_op_table(boole_and), -1 %),
    inline_logeqv   =   Inline_reduce(% boole_op_table(boole_eqv), -1 %),
    inline_logior   =   Inline_reduce(% boole_op_table(boole_ior), 0 %),
    inline_logxor   =   Inline_reduce(% boole_op_table(boole_xor), 0 %),
    );


define inline_boole(args, nresults);
    lvars op;
    Check_nargs(args, 3, islistlength);
    destpair(args) -> args -> op;
    if (is_constant_value(op) ->> op) then
        checkr_boole_op(op) -> op;
        lispSET_STKLEN(nresults, 1) -> nresults;
        compile_args(args) ->;
        sysCALLQ(op);
        lispRESET_STKLEN(nresults)
    else
        ABORT_CALLI
    endif
enddefine;


/* Character comparisons */

define lconstant Inline_char_<>(args, nresults, pred);
    lvars item;
    [% for item in_cl_list args do
        [^@CHAR-CODE ^item]
    endfor %] -> args;
    Inline_num_compare(args, nresults, pred)
enddefine;


constant procedure (
    inline_char_<   =   Inline_char_<>(% nonop fi_<  %),
    inline_char_<=  =   Inline_char_<>(% nonop fi_<= %),
    inline_char_=   =   Inline_char_<>(% nonop ==    %),
    inline_char_>   =   Inline_char_<>(% nonop fi_>  %),
    inline_char_>=  =   Inline_char_<>(% nonop fi_>= %),
    );


/* List & vector constructors etc */

define inline_list(args, nresults);
    lvars n;
    listlength(args) -> n;
    Check_space(n);
    lispSET_STKLEN(nresults, 1) -> nresults;
    compile_args(args) ->;
    lispPUSHQ([]);
    repeat n times
        sysCALLQ(conspair)
    endrepeat;
    lispRESET_STKLEN(nresults)
enddefine;


define inline_list_*(args, nresults);
    lvars n;
    Check_nargs(args, 1, listlength_>=);
    Inline_reduce(args, nresults, conspair, false)
enddefine;


define inline_vector(args, nresults);
    lispSET_STKLEN(nresults, 1) -> nresults;
    sysPUSHQ(compile_args(args));
    sysCALLQ(consvector);
    lispRESET_STKLEN(nresults)
enddefine;


define inline_concatenate(args, nresults);
    lvars type, key;
    Check_nargs(args, 1, listlength_>=);
    destpair(args) -> (type, args);
    if (is_constant_value(type) ->> type)
    and (stype_->_key(type) -> type ->> key)
    and atom(type) then
        lispSET_STKLEN(nresults, 1) -> nresults;
        procedure();
            lvars sl, for_string, arg, val;
            INIT_STK_VARS;
            lispNEW_VTOK() -> sl;
            sysCALLQ(stacklength);
            lispPOP(sl);
            key == string_key -> for_string;
            for arg in_cl_list args do
                if (is_constant_value(arg) ->> val)
                and (seq_length(val) <= 3) then
                    appdata({% explode_seq(val, for_string) %}, lispPUSHQ)
                else
                    compile_form(arg, 1);
                    lispPUSHQ(for_string);
                    sysCALLQ(explode_seq)
                endif
            endfor;
            sysCALLQ(stacklength);
            lispPUSH(sl);
            sysCALLQ(nonop fi_-);
            sysCALLQ(
                if key == pair_key then
                    conslist
                else
                    class_cons(key)
                endif);
            sl -> lispNEW_VTOK()
        endprocedure();
        lispRESET_STKLEN(nresults)
    else
        ABORT_CALLI
    endif
enddefine;


/* Structure access */

define inline_aref(args, nresults);
    lvars n, avar;
    listlength(args) -> n;
    Check_nargs(n, 1, nonop >=);
    lispSET_STKLEN(nresults, 1) -> nresults;
    if n == 2 then
        compile_args(args) ->;
        sysCALLQ(aref1)
    else
        lispNEW_VTOK() -> avar;
        compile_form(destpair(args) -> args, 1);
        lispPOP(avar);
        compile_args(args) ->;
        sysCALL(avar);
        avar -> lispNEW_VTOK()
    endif;
    lispRESET_STKLEN(nresults)
enddefine;


define updaterof inline_aref(args, nresults);
    lvars n, avar;
    dlocal Stackoffset;

    listlength(args) -> n;
    Check_nargs(n, 2, nonop >=);
    lispSET_STKLEN(nresults, 0) -> nresults;
    if n == 3 then
        compile_args(args) ->;
        sysUCALLQ(aref1)
    else
        lispNEW_VTOK() -> avar;
        compile_arg(destpair(args) -> args);
        compile_arg(destpair(args) -> args);
        lispPOP(avar);
        Stackoffset - 1 -> Stackoffset;
        applist(args, compile_arg);
        sysUCALL(avar);
        avar -> lispNEW_VTOK()
    endif;
    lispRESET_STKLEN(nresults)
enddefine;


define inline_nth(args, nresults, n, key);
    Check_nargs(args, 1, islistlength);
    lispSET_STKLEN(nresults, 1) -> nresults;
    compile_args(args) ->;
    lispNTH(n, key);
    lispRESET_STKLEN(nresults)
enddefine;


define updaterof inline_nth(args, nresults, n, key);
    Check_nargs(args, 2, islistlength);
    lispSET_STKLEN(nresults, 0) -> nresults;
    compile_args(args) ->;
    lispU_NTH(n, key);
    lispRESET_STKLEN(nresults)
enddefine;


constant procedure (
    inline_car      =   Inline_any(% fast_front, 1 %),
    inline_cdr      =   Inline_any(% fast_back, 1 %),
    inline_caar     =   Inline_any(% conspair(fast_front, fast_front), 1 %),
    inline_cadr     =   Inline_any(% conspair(fast_back, fast_front), 1 %),
    inline_cdar     =   Inline_any(% conspair(fast_front, fast_back), 1 %),
    inline_cddr     =   Inline_any(% conspair(fast_back, fast_back), 1 %),
    inline_rplaca   =   Inline_any(% fast_rplaca, 2 %),
    inline_rplacd   =   Inline_any(% fast_rplacd, 2 %),
    inline_schar    =   Inline_any(% fast_schar, 2 %),
;;; inline_svref    =   Inline_any(% fast_subscrv0, 2 %), /* see note below */
    inline_char_code=   Inline_any(% fast_char_code, 1 %),
    inline_code_char=   Inline_any(% fast_code_char, 1 %),
    inline_char_int =   Inline_any(% fast_char_code, 1 %),
    );

/* Note: fast_subscrv0 takes its arguments the other way round to SVREF,
    but it still works (cos vector + offset = offset + vector)
*/

/* Predicates */

define inline_equal(args, nresults);

    define lconstant Eq_simple(item);
        (is_constant_value(item) ->> item)
            and
        (issimple(item) or issymbol(item))
    enddefine;

    if islistlength(args, 2)
    and (Eq_simple(cadr(args)) or Eq_simple(car(args))) then
        lispSET_STKLEN(nresults, 1) -> nresults;
        compile_args(args) ->;
        sysCALLQ(nonop ==);
        lispTRUE();
        lispRESET_STKLEN(nresults)
    else
        ABORT_CALLI
    endif
enddefine;


/* Lists */

constant procedure (
    inline_append   =   Inline_reduce(% append, [] %),
    inline_nconc    =   Inline_reduce(% nconc, [] %),
    );


/* Funcall, Apply, and Complement */

define inline_funcall(args, nresults);
    lvars fn, CALL;
    dlocal Tmp_ftok_list;
    Check_nargs(args, 1, listlength_>=);
    destpair(args) -> args -> fn;
    compile_fn(fn) -> fn -> CALL;
    lispSET_STKLEN(nresults, f_results(fn)) -> nresults;
    CALL(fn, compile_args(args), nresults)
enddefine;


define inline_apply(args, nresults);
    lvars fn, CALL, n;
    dlocal Tmp_ftok_list;
    Check_nargs(args, 2, listlength_>=);
    destpair(args) -> args -> fn;
    compile_fn(fn) -> fn -> CALL;
    lispSET_STKLEN(nresults, f_results(fn)) -> nresults;
    compile_args(args) - 1 -> n;
    sysCALLQ(destlist);
    sysPUSHQ(n);
    sysCALLQ(nonop fi_+);
    CALL(fn, false, nresults)
enddefine;


define inline_complement(args, nresults);
    lvars fn, p;
    Check_nargs(args, 1, islistlength);
    compile_fn(front(args)) -> fn ->;
    if isprocedure(fn)
    and not(is_protected_closure(fn)) /* I.e. procedure compilation record */
    then
        if pdpart(fn) == check_fmin_apply
        and pdpart(frozval(1, fn) ->> p) == lisp_true_apply then
            complement_function_info(fn);
            check_fmin_apply(%
                lisp_not_true_apply(% frozval(1, p) %),
                frozval(2, fn) %)
                -> fn;
            -> function_info(fn)
        else
            complement(fn) -> fn
        endif;
        lispPUSHNQ(fn, nresults)
    else
        lispSET_STKLEN(nresults, 1) -> nresults;
        sysPUSH("ident complement_apply");
        if isprocedure(fn) then
            sysPUSHQ(fn)
        else
            sysPUSH(fn)
        endif;
        sysPUSHQ(1);
        sysCALLQ(consclosure);
        lispRESET_STKLEN(nresults)
    endif
enddefine;


define inline_constantly(args, nresults);
    lvars val;
    Check_nargs(args, 1, islistlength);
    if (is_constant_value(front(args)) ->> val) then
        lispPUSHNQ(constantly(val), nresults)
    else
        ABORT_CALLI
    endif
enddefine;


/* Values */

define inline_values(args, nresults);
    lispSET_STKLEN(nresults, listlength(args)) -> nresults;
    compile_args(args) ->;
    lispRESET_STKLEN(nresults)
enddefine;


/* Typep */

define inline_typep(args, nresults);
    lvars type, pdr;
    Check_nargs(args, 2, islistlength);
    if (is_constant_value(cadr(args)) ->> type)
    and (is_basic_system_type(type) ->> pdr) then
        lispSET_STKLEN(nresults, 1) -> nresults;
        compile_form(car(args), 1);
        sysCALLQ(pdr);
        lispTRUE();
        lispRESET_STKLEN(nresults)
    else
        ABORT_CALLI
    endif
enddefine;


/* Pop */

define inline_pop(args, nresults);
    Check_nargs(args, 1, islistlength);
    front(args) -> args;
    if issymbol(args) and not(symbol_macro(args)) then
        if deref(nresults) == 0 then
            lispPUSH(args);
            sysCALLQ(if Fast then fast_back else cdr endif);
            lispPOP(args)
        else
            lispSET_STKLEN(nresults, 1) -> nresults;
            lispPUSH(args);
            if Fast then
                sysCALLQ(fast_destpair)
            else
                sysCALLQ(car);
                lispPUSH(args);
                sysCALLQ(fast_back)
            endif;
            lispPOP(args);
            lispRESET_STKLEN(nresults)
        endif
    else
        ABORT_CALLI
    endif
enddefine;


/* Get */

define inline_get(args, nresults);
    lvars n;
    listlength(args) -> n;
    unless n == 2 or n == 3 do
        ABORT_CALLI
    endunless;
    lispSET_STKLEN(nresults, 1) -> nresults;
    compile_args(args) ->;
    if n == 2 then
        lispPUSHQ(nil)
    endif;
    sysCALLQ(fast_get);
    lispRESET_STKLEN(nresults)
enddefine;


define inline_set(args, nresults);
    lvars sym, nr;
    /* Can optimise (set sym val) to lispPOP(sym) if sym is quoted,
        has a perm ident, and is not currently lexical or symbol macro.
    */
    Check_nargs(args, 2, islistlength);
    destpair(args) -> (sym, args);
    if issymbol(is_constant_value(sym) ->> sym)
    and not(symbol_macro(sym))
    and isident(sys_current_ident(sym)) == "perm" then
        compile_form(front(args), 1);
        deref(nresults) -> nr;
        if nr == 1 or nr == false then
            sysPUSHS(1);
            lispPOP(sym)
        else
            lispPOP(sym);
            lispPUSHN(sym, nresults)
        endif
    else
        ABORT_CALLI
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan 17 1996
        Added inline_set.
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Jun 13 1995
        Added inline_concatenate and removed inline_setf.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Apr 26 1994
        Changes to inline_setf and inline_pop for function names and symbol
        macros.
--- John Williams, Aug 31 1993
        Inline_reduce and related procedures now only compile inline code for
        calls with 5 or less arguments. Tidied up.
--- John Williams, Aug 27 1993
        Added inline_char_< and friends, also inline_rassoc.
--- John Williams, Jan 19 1989
        Re-wrote -Inline_<>-  (cf SFR 4260).
 */
