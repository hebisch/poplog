/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/prolog_eval.p
 > Purpose:         Prolog: evaluation of terms as Pop-11 expressions
 > Author:          Robert Duncan, Apr 29 1993
 > Documentation:
 > Related Files:
 */


section prolog;

constant procedure ( bad_goal );

;;; ========================================================================

;;; prolog_doeval:
;;;     interprets a Prolog term as a Pop-11 expression

define prolog_doeval(/* term */) with_nargs 1;
    lvars term = prolog_deref(/* term */);
    returnif(issimple(term))(term);
    lvars (fn, arity) = prolog_termspec(term);
    if arity == 0 then
        term;
    elseif fn == "quote" then
        prolog_args(term);
    elseif fn == "quote_nd" then
        prolog_args_nd(term);
    elseif fn == "-" and arity == 1 then
        negate(prolog_doeval(fast_prolog_arg(1, term)));
    elseif fn == "." and arity == 2 then
        conspair(prolog_doeval(prolog_arg_nd(1, term)),
                 prolog_doeval(prolog_arg_nd(2, term)));
    else
        valof(fn)(prolog_appargs_nd(term, prolog_doeval));
    endif;
enddefine;

;;; prolog_eval/1:
;;;     evaluate a term as a Pop-11 expression (always succeeds)

define prolog_eval\/1(/* term */) with_nargs 1;
    prolog_doeval(/* term */);
    chain(prolog_apply_continuation);
enddefine;

;;; prolog_eval/2:
;;;     evaluate a term as a Pop-11 expression and unify the result

define prolog_eval\/2(/* term, */ X) with_nargs 2;
    lvars X;
    if prolog_unify(prolog_doeval(/* term */), X) then
        chain(prolog_apply_continuation);
    endif;
enddefine;

;;; prolog_evaltrue/1:
;;;     evaluate a term as a Pop-11 expression and succeed if the result
;;;     is non-false

define prolog_evaltrue\/1(/* term */) with_nargs 1;
    if prolog_doeval(/* term */) then
        chain(prolog_apply_continuation);
    endif;
enddefine;

;;; prolog_setq/2:
;;;     evaluate a term and assign the result to a Pop-11 permanent
;;;     variable

define prolog_setq\/2(/* var, */ term) with_nargs 2;
    lvars var = prolog_deref(/* var */), term;
    if isprologvar(var) then
        bad_goal(var, term, "prolog_setq", 2);
    endif;
    prolog_doeval(term) -> term;
    ;;; allow the variable to be declared quietly
    dlocal prwarning = erase;
    prolog_generalise(term) -> valof(var);
    chain(prolog_apply_continuation);
enddefine;

;;; prolog_val/2:
;;;     unify the value of a Pop-11 permanent variable

define prolog_val\/2(/* var, */ X) with_nargs 2;
    lvars var = prolog_deref(/* var */), X;
    if isprologvar(var) then
        bad_goal(var, X, "prolog_val", 2);
    endif;
    if prolog_unify(prolog_instance(valof(var)), X) then
        chain(prolog_apply_continuation);
    endif;
enddefine;

;;; Pop-11 definitions for Prolog bitwise operators

define \\\/ = nonop||(%%) enddefine;
define \/\\ = nonop&&(%%) enddefine;
define \\   = nonop~~(%%) enddefine;

endsection;     /* prolog */
