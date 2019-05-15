/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/predicate_abolish.p
 > Purpose:         Prolog: abolish a predicate definition
 > Author:          Robert John Duncan, Jul  5 1993
 > Documentation:
 > Related Files:
 */

section prolog;

constant
    procedure ( alias, predicate_table, is_special_functor,
        cons_undefined_closure, );

;;; ========================================================================

define predicate_abolish(fn, arity, system);
    lvars fn, arity, system;

    define lconstant abolish_error(fn, arity);
        lvars fn, arity;
        mishap(prolog_maketerm(fn, arity, "/", 2), 1,
            'TRYING TO ABOLISH A SYSTEM PREDICATE');
    enddefine;

    if is_special_functor(fn, arity) then
        abolish_error(fn, arity);
    endif;
    lvars name = alias(fn, arity), id = isdefined(name);
    returnunless(id);
    lvars pred = predicate_table(id);
    if pred and pred_system(pred) == true or isconstant(id) then
        unless system then abolish_error(fn, arity) endunless;
    else
        false -> system;
    endif;
    ;;; eradicate any declarations for the predicate
    false -> predicate_table(id);
    ;;; zap its definition
    if system then
        ;;; abolishing a system predicate: definition can't be changed,
        ;;; but cancelling the identifier will allow the name to be
        ;;; redefined
        syscancel(name);
    else
        cons_undefined_closure(fn, arity) -> idval(id);
    endif;
enddefine;

endsection;     /* prolog */
