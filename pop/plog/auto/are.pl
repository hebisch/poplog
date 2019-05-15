/*  --- Copyright University of Sussex 1991. All rights reserved. ----------
 >  File:           C.all/plog/auto/are.pl
 >  Purpose:        generalisation of IS to handle more than one result
 >  Author:         Chris Mellish, July 1983 (see revisions)
 >  Documentation:  HELP * PLOGTOPOP
 >  Related Files:
 */

;;; Conventions for expressions are as for PROLOG_EVAL and IS
;;; The result is always a list.

:- prolog_language("pop11").

define prolog_eval_expr(x);
    lvars x, n;
    prolog_deref(x) -> x;
    if prolog_complexterm(x) then
        if prolog_predword(x) == "quote" then
            for n from 1 to prolog_nargs(x) do
                prolog_arg(n,x)
            endfor
        elseif prolog_predword(x) == "." and prolog_nargs(x) == 2 then
            conspair(prolog_eval_expr(prolog_arg(1, x)),
                prolog_eval_expr(prolog_arg(2, x)))
        else
            for n from 1 to prolog_nargs(x) do
                prolog_eval_expr(prolog_arg(n, x))
            endfor;
            apply(valof(prolog_predword(x)))
        endif
    else
        x
    endif
enddefine;

define prolog_collect_eval(expr);
    lvars expr;
    [% prolog_eval_expr(expr) %]
enddefine;

procedure(x, y, contn);
    lvars x, y, contn;
    chain(prolog_collect_eval(y), x, contn, unify)
endprocedure -> prolog_valof("are", 2);

:- prolog_language("prolog").

:- current_op(Prec, xfx, is), op(Prec, xfx, are).

/* --- Revision History ---------------------------------------------------
--- Simon Nichols, Sep 19 1991
        Declared pararmeters as lvars in all procedures.
        Some tidying up.
--- Robert John Duncan, Nov  2 1990
        Changed "vars" to "lvars" in -prolog_eval_expr-
 */
