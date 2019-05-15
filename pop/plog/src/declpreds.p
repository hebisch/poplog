/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/declpreds.p
 > Purpose:         Prolog: predicate declarations
 > Author:          Robert John Duncan, Jul  5 1993 (see revisions)
 > Documentation:
 > Related Files:
 */

section prolog;

constant
    procedure ( declare_global, declare_import, declare_export,
        declare_dynamic, declare_static, declare_clauses, declare_no_clauses,
        declare_system, declare_user, declare_hidden, declare_revealed, );

endsection;     /* prolog */

PROLOG

:- op(254, fx, [
        'global',
        'import',
        'export',
        'dynamic',
        'static',
        'clauses',
        'no_clauses',
        'system_predicate',
        'user_predicate']).

:- module prolog.

'global'(Spec) :-
    predicate_spec(Spec, Fn, Arity),
    declare_global(Fn, Arity),
    fail.
'global'(_).

'import'(Spec) :-
    predicate_spec(Spec, Fn, Arity),
    declare_import(Fn, Arity),
    fail.
'import'(_).

'export'(Spec) :-
    predicate_spec(Spec, Fn, Arity),
    declare_export(Fn, Arity),
    fail.
'export'(_).

'dynamic'(Spec) :-
    predicate_spec(Spec, Fn, Arity),
    declare_dynamic(Fn, Arity),
    fail.
'dynamic'(_).

'static'(Spec) :-
    predicate_spec(Spec, Fn, Arity),
    declare_static(Fn, Arity),
    fail.
'static'(_).

'clauses'(Spec) :-
    predicate_spec(Spec, Fn, Arity),
    declare_clauses(Fn, Arity),
    fail.
'clauses'(_).

'no_clauses'(Spec) :-
    predicate_spec(Spec, Fn, Arity),
    declare_no_clauses(Fn, Arity),
    fail.
'no_clauses'(_).

'system_predicate'(Spec) :-
    predicate_spec(Spec, Fn, Arity),
    declare_system_predicate(Fn, Arity),
    fail.
'system_predicate'(_).

'user_predicate'(Spec) :-
    predicate_spec(Spec, Fn, Arity),
    declare_user_predicate(Fn, Arity),
    fail.
'user_predicate'(_).

prolog_hide(Spec) :-
    predicate_spec(Spec, Fn, Arity),
    declare_hidden(Fn, Arity),
    fail.
prolog_hide(_).

prolog_reveal(Spec) :-
    predicate_spec(Spec, Fn, Arity),
    declare_revealed(Fn, Arity),
    fail.
prolog_reveal(_).

predicate_spec(Spec, _, _) :-
    var(Spec),
    !,
    predicate_spec_error(Spec).
predicate_spec('/'(Fn,Arity), Fn, Arity) :-
    atom(Fn),
    integer(Arity),
    !.
predicate_spec((Spec,Specs), Fn, Arity) :-
    predicate_spec(Spec, Fn, Arity).
predicate_spec((Spec,Specs), Fn, Arity) :-
    !,
    predicate_spec(Specs, Fn, Arity).
predicate_spec(Spec, _, _) :-
    predicate_spec_error(Spec).

predicate_spec_error(Spec) :-
    error('ILLEGAL PREDICATE SPECIFICATION', [Spec]).

declare_global(Fn, Arity) :-
    prolog_eval(declare_global(quote(Fn, Arity))).

declare_import(Fn, Arity) :-
    prolog_eval(declare_import(quote(Fn, Arity))).

declare_export(Fn, Arity) :-
    prolog_eval(declare_export(quote(Fn, Arity))).

declare_dynamic(Fn, Arity) :-
    prolog_eval(declare_dynamic(quote(Fn, Arity))).

declare_static(Fn, Arity) :-
    prolog_eval(declare_static(quote(Fn, Arity))).

declare_clauses(Fn, Arity) :-
    prolog_eval(declare_clauses(quote(Fn, Arity))).

declare_no_clauses(Fn, Arity) :-
    prolog_eval(declare_no_clauses(quote(Fn, Arity))).

declare_system_predicate(Fn, Arity) :-
    prolog_eval(declare_system(quote(Fn, Arity))).

declare_user_predicate(Fn, Arity) :-
    prolog_eval(declare_user(quote(Fn, Arity))).

declare_hidden(Fn, Arity) :-
    prolog_eval(declare_hidden(quote(Fn, Arity))).

declare_revealed(Fn, Arity) :-
    prolog_eval(declare_revealed(quote(Fn, Arity))).

:- endmodule prolog.


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug  4 1993
        Added support for hide and reveal
 */
