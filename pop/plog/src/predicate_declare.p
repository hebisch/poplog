/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/predicate_declare.p
 > Purpose:         Prolog: predicate declarations
 > Author:          Robert John Duncan, Jul  5 1993 (see revisions)
 > Documentation:
 > Related Files:
 */

section prolog;

constant
    procedure ( pred_isdefined, proc_clauses, is_dynamic, pred_spec,
        cons_undefined_closure, comp_static_predicate, cons_dynamic_closure,
        comp_dynamic_predicate, insert_at_end, is_special_functor,
        predicate_record, pred_declare, pred_valof, );

;;; ========================================================================

;;; make_static:
;;;     give a predicate a static value

define make_static(pred);
    lvars pred, proc, clauselist;
    if (pred_isdefined(pred) ->> proc)
    and (is_dynamic(proc) ->> clauselist)
    then
        Back(clauselist) -> clauselist;
        if clauselist == [] then
            cons_undefined_closure(pred_spec(pred)) -> proc;
        else
            lvars clause;
            comp_static_predicate([%
                For clause in clauselist do
                    prolog_instance(proc_clauses(clause));
                endfor %], pred) -> proc;
        endif;
        proc -> pred_valof(pred);
    endif;
enddefine;

;;; make_dynamic:
;;;     give a predicate a dynamic value

define make_dynamic(pred);
    lvars pred, proc;
    unless (pred_isdefined(pred) ->> proc)
    and is_dynamic(proc)
    then
        lvars clause, clos = cons_dynamic_closure(pred_spec(pred));
        if proc then
            For clause in proc_clauses(proc) do
                comp_dynamic_predicate(prolog_instance(clause), pred) -> clause;
                insert_at_end(clause, clos);
            endfor;
        endif;
        clos -> pred_valof(pred);
    endunless;
enddefine;

define lconstant declaration_error(fn, arity, old_type, new_type);
    lvars fn, arity, old_type, new_type;
    mishap(prolog_maketerm(fn, arity, "/", 2), 1, lowertoupper(old_type)
        <> ' PREDICATE CANNOT BE DECLARED ' <> new_type);
enddefine;

define declare_dynamic(fn, arity);
    lvars fn, arity;
    if is_special_functor(fn, arity) then
        declaration_error(fn, arity, 'system', 'dynamic');
    endif;
    ;;; get a predicate record
    lvars pred = predicate_record(fn, arity, true);
    ;;; set dynamic attribute
    returnif(pred_dynamic(pred) == true);
    if pred_system(pred) == true then
        declaration_error(fn, arity, 'system', 'dynamic');
    endif;
    true -> pred_dynamic(pred);
    ;;; dynamic implies user_predicate
    false -> pred_system(pred);
    ;;; declare an identifier
    pred_declare(pred);
    ;;; make the value dynamic
    make_dynamic(pred);
enddefine;

define declare_static(fn, arity);
    lvars fn, arity;
    returnif(is_special_functor(fn, arity));
    ;;; get a predicate record
    lvars pred = predicate_record(fn, arity, true);
    ;;; set static attribute
    returnif(pred_dynamic(pred) == false);
    false -> pred_dynamic(pred);
    ;;; declare an identifier
    pred_declare(pred);
    ;;; make the value static
    make_static(pred);
enddefine;

define declare_system(fn, arity);
    lvars fn, arity;
    returnif(is_special_functor(fn, arity));
    ;;; get a predicate record
    lvars pred = predicate_record(fn, arity, true);
    ;;; set system attribute
    returnif(pred_system(pred) == true);
    if pred_dynamic(pred) == true then
        declaration_error(fn, arity, 'dynamic', 'system_predicate');
    endif;
    true -> pred_system(pred);
    ;;; system_predicate implies static
    false -> pred_dynamic(pred);
    ;;; declare an identifier
    pred_declare(pred);
enddefine;

define declare_user(fn, arity);
    lvars fn, arity;
    if is_special_functor(fn, arity) then
        declaration_error(fn, arity, 'system', 'user_predicate');
    endif;
    ;;; get a predicate record
    lvars pred = predicate_record(fn, arity, true);
    ;;; set user attribute
    returnif(pred_system(pred) == false);
    if pred_system(pred) == true then
        declaration_error(fn, arity, 'system', 'user_predicate');
    endif;
    false -> pred_system(pred);
    ;;; declare an identifier
    pred_declare(pred);
enddefine;

define declare_no_clauses(fn, arity);
    lvars fn, arity;
    returnif(is_special_functor(fn, arity));
    ;;; get a predicate record
    lvars pred = predicate_record(fn, arity, true);
    ;;; set notext attribute
    returnif(pred_notext(pred) == true);
    true -> pred_notext(pred);
    ;;; declare an identifier
    pred_declare(pred);
    ;;; zap any existing clauses
    [] -> proc_clauses(pred_valof(pred));
enddefine;

define declare_clauses(fn, arity);
    lvars fn, arity;
    ;;; ignore this odd case
    returnif(is_special_functor(fn, arity));
    ;;; get a predicate record
    lvars pred = predicate_record(fn, arity, true);
    ;;; set text attribute
    returnif(pred_notext(pred) == false);
    false -> pred_notext(pred);
    ;;; declare an identifier
    pred_declare(pred);
enddefine;

define declare_hidden(fn, arity);
    lvars fn, arity;
    ;;; get a predicate record
    lvars pred = predicate_record(fn, arity, true);
    ;;; set hidden attribute
    returnif(pred_hidden(pred));
    true -> pred_hidden(pred);
    ;;; declare an identifier
    pred_declare(pred);
enddefine;

define declare_revealed(fn, arity);
    lvars fn, arity;
    ;;; get a predicate record
    lvars pred = predicate_record(fn, arity, true);
    ;;; unset hidden attribute
    returnunless(pred_hidden(pred));
    false -> pred_hidden(pred);
    ;;; declare an identifier
    pred_declare(pred);
enddefine;

endsection;     /* prolog */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug  4 1993
        Added support for hide and reveal
 */
