/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/obsolete.p
 > Purpose:         Prolog: ancient and unwanted definitions
 > Author:          Robert John Duncan, May 13 1993
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

section prolog =>

    ;;; obsolete procedures defined here
    no_prolog_defn,
    prolog_apppredicates_defined,
    prolog_define,
    prolog_nargs,
    prolog_predword,
    prolog_predicates_defined,
    prolog_safereadline,
    prolog_undefined,
    unify,

    ;;; obsolete procedures defined elsewhere but only exported here
    prolog_assert,              ;;; dbpreds.p
    prolog_remove,              ;;; dbpreds.p
    prolog_system_procedure,    ;;; systempreds.p

    ;;; these next aren't really obsolete -- they're still used in the
    ;;; system -- but are only exported to preserve old programs
    isprolog_variable_tok,
    prolog_var_no,
;

define prolog_nargs =
    prolog_arity(%%);
enddefine;

define prolog_predword =
    prolog_functor(%%);
enddefine;

define unify =
    prolog_unifyc(%%);
enddefine;

define prolog_undefined =
    undefined\/0(%%);
enddefine;

define prolog_safereadline =
    prolog_readline(%%);
enddefine;

define prolog_define(clauses);
    lvars clauses;
    lvars (fn, arity) = prolog_termspec(prolog_head(hd(clauses)));
    lvars pred = predicate_record(fn, arity, true);
    if pred_dynamic(pred) == "undef" then
        false -> pred_dynamic(pred);
    endif;
    if pred_system(pred) == "undef" then
        prolog_syspredicate -> pred_system(pred);
    endif;
    if pred_notext(pred) == "undef" then
        prolog_no_clauses -> pred_notext(pred);
    endif;
    pred_declare(pred);
    converter_to_contn(% comp_static_predicate(clauses, pred), arity %);
enddefine;

define no_prolog_defn(fn, arity);
    lvars fn, arity;
    not(isword(fn) and predicate_isdefined(fn, arity));
enddefine;

define prolog_predicates_defined(fn);
    lvars fn;
    [% app_named_predicates(fn, pred_arity) %];
enddefine;

define prolog_apppredicates_defined(p);
    lvars p;
    app_predicates(pred_spec <> p);
enddefine;

endsection;     /* prolog */
