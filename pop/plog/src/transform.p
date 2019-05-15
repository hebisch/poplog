/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/plog/src/transform.p
 > Purpose:         Prolog: transform body goals prior to compilation
 > Author:          Robert Duncan, Apr 30 1993 (see revisions)
 > Documentation:
 > Related Files:
 */


section prolog;

constant
    procedure ( prolog_conjunction\/2, prolog_disjunction\/2,
        prolog_conditional\/3, prolog_fail_if\/1, prolog_cut\/1,
        predicate_record, pred_declare, is_undefined_closure,
        cons_undefined_closure, ),
;

vars procedure ( pred_idval, );

;;; ========================================================================

define transform_body(body);
    lvars body, cut_var = false;

    ;;; declare a predicate referenced in the body of a clause
    define lconstant declare(fn, arity) -> name;
        lvars fn, arity, name;
        lvars pred = predicate_record(fn, arity, true);
        pred_declare(pred);
        word_identifier(pred_alias(pred), current_section, true) -> name;
        unless pred_idval(pred) then
            ;;; not yet given a proper value
            unless isconstant(name) then
                cons_undefined_closure(fn, arity) -> pred_idval(pred);
            endunless;
        endunless;
    enddefine;

    ;;; declare a Pop-11 identifier refenced in evaluable code
    define lconstant pop_declare(w) -> w;
        lvars w;
        if isword(w) then
            sysdeclare(w);
            word_identifier(w, current_section, true) -> w;
        endif;
    enddefine;

    define lconstant transform_expression(expr);
        lvars expr;
        lvars (fn, arity) = prolog_termspec(expr);
        returnif(arity == 0 or fn == "quote" or fn == "quote_nd")(expr);
        if fn == "." and arity == 2 then
            conspair(
                transform_expression(prolog_arg(1, expr)),
                transform_expression(prolog_arg(2, expr)));
        elseif fn == "-" and arity == 1 then
            consprologterm(
                transform_expression(prolog_arg(1, expr)),
                "negate", 1);
        elseif fn == "valof" and arity == 1 then
            consprologterm(
                pop_declare(prolog_arg(1, expr)),
                "valof", 1);
        else
            consprologterm(
                prolog_appargs(expr, transform_expression),
                pop_declare(fn), arity);
        endif;
    enddefine;

    define lconstant transform_goal(goal);
        lvars goal;
        lvars (fn, arity) = prolog_termspec(goal);
        if isprologvar(fn) then
            consprologterm(fn, "ident call\/1", 1);
        elseif fn == "\\\+" and arity == 1 then
            consprologterm(
                transform_goal(prolog_arg(1, goal)),
                "ident prolog_fail_if\/1", 1);
        elseif fn == "," and arity == 2 then
            consprologterm(
                transform_goal(prolog_arg(1, goal)),
                transform_goal(prolog_arg(2, goal)),
                "ident prolog_conjunction\/2", 2);
        elseif fn == ";" and arity == 2 then
            lvars arg = prolog_arg(1, goal);
            if prolog_checkspec(arg, "->", 2) then
                ;;; conditional
                consprologterm(
                    transform_goal(prolog_arg(1, arg)),
                    transform_goal(prolog_arg(2, arg)),
                    transform_goal(prolog_arg(2, goal)),
                    "ident prolog_conditional\/3", 3);
            else
                consprologterm(
                    transform_goal(arg),
                    transform_goal(prolog_arg(2, goal)),
                    "ident prolog_disjunction\/2", 2);
            endif;
        elseif fn == "!" and arity == 0 then
            unless cut_var then
                prolog_newvar() -> cut_var;
            endunless;
            consprologterm(cut_var, "ident prolog_cut\/1", 1);
        elseif fn == "prolog_eval" and arity == 1 then
            consprologterm(
                transform_expression(prolog_arg(1, goal)),
                "ident prolog_eval\/1", 1);
        elseif fn == "prolog_eval" and arity == 2 then
            consprologterm(
                transform_expression(prolog_arg(1, goal)),
                prolog_arg(2, goal),
                "ident prolog_eval\/2", 2);
        elseif fn == "prolog_evaltrue" and arity == 1 then
            consprologterm(
                transform_expression(prolog_arg(1, goal)),
                "ident prolog_evaltrue\/1", 1);
        elseif fn == "prolog_setq" and arity == 2 then
            consprologterm(
                pop_declare(prolog_arg(1, goal)),
                transform_expression(prolog_arg(2, goal)),
                "ident prolog_setq\/2", 2);
        elseif fn == "prolog_val" and arity == 2 then
            consprologterm(
                pop_declare(prolog_arg(1, goal)),
                prolog_arg(2, goal),
                "ident prolog_val\/2", 2);
        else
            ;;; general call
            consprologterm(
                prolog_args_nd(goal),
                declare(fn, arity), arity);
        endif;
    enddefine;

    define lconstant transform_body_goals(/* goal */) with_nargs 1;
        lvars goal = prolog_deref(/* goal */);
        lvars (fn, arity) = prolog_termspec(goal);
        ;;; flatten out conjunctions
        while fn == "," and arity == 2 do
            transform_body_goals(fast_prolog_arg(1, goal));
            prolog_arg(2, goal) -> goal;
            prolog_termspec(goal) -> (fn, arity);
        endwhile;
        ;;; special cases
        if fn == "true" and arity == 0 then
            ;;; ignore
        elseif fn == "!" and arity == 0 and not(cut_var) then
            fn;
        else
            transform_goal(goal);
        endif;
    enddefine;

    ([% transform_body_goals(body) %], cut_var);
enddefine;

endsection;     /* prolog */

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jan 27 1994
        Fixed a bug in transformation of conditional form (P -> X;Y)
 */
