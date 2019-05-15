/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/predicate_define.p
 > Purpose:         Prolog: defining predicates
 > Author:          Robert John Duncan, Jul  5 1993
 > Documentation:
 > Related Files:
 */

include vm_flags

section prolog;

constant
    procedure ( is_special_functor, predicate_record, comp_dynamic_predicate,
        comp_static_predicate, insert_at_end, pred_declare, pred_isdefined,
        is_dynamic, cons_undefined_closure, declare_dynamic, pred_valof, );

;;; ========================================================================

vars
    prolog_syspredicate = false,    ;;; user predicates by default
    prolog_no_clauses   = false,    ;;; save clauses by default
;

define predicate_define(fn, arity, clauses, tag, reconsulting);
    lvars fn, arity, clauses, tag, reconsulting;

    ;;; if this predicate has been referenced already it will have been
    ;;; forward declared as vars, but it might now be declared constant
    ;;; so set the VM flags to allow this
    dlocal  pop_vm_flags = pop_vm_flags &&~~ VM_PERM_FIXED_DECLARE;

    define lconstant redefinition_error(fn, arity);
        lvars fn, arity;
        mishap('TRYING TO REDEFINE A SYSTEM PREDICATE', [^fn / ^arity]);
    enddefine;

    ;;; check for special functor, e.g. call/N for any N
    if is_special_functor(fn, arity) then redefinition_error(fn, arity) endif;
    ;;; get a predicate record
    lvars pred = predicate_record(fn, arity, true);
    ;;; fill in any undeclared attributes of the predicate record
    if pred_dynamic(pred) == "undef" then
        false -> pred_dynamic(pred);
    endif;
    if pred_system(pred) == "undef" then
        lvars id = pred_ident(pred);
        not(pred_dynamic(pred)) and prolog_syspredicate
        or id and isconstant(id) -> pred_system(pred);
    endif;
    if pred_notext(pred) == "undef" then
        not(pred_dynamic(pred)) and prolog_no_clauses -> pred_notext(pred);
    endif;
    ;;; declare the predicate identifier
    pred_declare(pred);
    ;;; if it already has a definition, check it's ok to redefine
    lvars proc = pred_isdefined(pred), dynamic = pred_dynamic(pred);
    if proc then
        ;;; already has a value
        if pred_system(pred) or not(isassignable(pred_ident(pred))) then
            redefinition_error(fn, arity);
        elseif reconsulting and pred_tag(pred) /== tag then
            ;;; delete existing definition to reclaim space
            lvars clauselist;
            if is_dynamic(proc) ->> clauselist then
                ;;; zap existing clauses
                [] ->> Front(clauselist) -> Back(clauselist);
                true -> dynamic;
            else
                cons_undefined_closure(fn, arity) -> pred_valof(pred);
            endif;
        else
            ;;; add clauses to an existing definition
            true -> dynamic;
        endif;
    endif;
    ;;; create the procedure value
    if dynamic then
        declare_dynamic(fn, arity);
        lvars clause, proc = pred_valof(pred);
        for clause in clauses do
            comp_dynamic_predicate(clause, pred) -> clause;
            insert_at_end(clause, proc);
        endfor;
    else
        comp_static_predicate(clauses, pred) -> proc;
    endif;
    ;;; bind to identifier
    proc -> pred_valof(pred);
    ;;; record where the procedure was defined
    tag -> pred_tag(pred);
enddefine;

endsection;     /* prolog */
