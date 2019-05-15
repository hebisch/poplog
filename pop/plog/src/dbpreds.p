/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/dbpreds.p
 > Purpose:         Prolog: operations on the database
 > Author:          Rob Duncan & Simon Nichols, Nov  3 1987 (see revisions)
 > Related Files:
 */

section prolog;

constant
    procedure ( predicate_record, pred_isdefined, is_dynamic,
        declare_dynamic, pred_valof, comp_dynamic_predicate, insert_at_front,
        insert_at_end, insert_at_posn, bad_goal, proc_clauses, delete_here,
        delete_at_posn, prolog_list, pred_spec, app_named_predicates,
        app_predicates, pred_has_clauses, );

;;; ========================================================================

/*
 *  Adding clauses to the database (assert)
 */

define lconstant assert(clause, insert);
    lvars clause, insert;
    lvars (fn, arity) = prolog_termspec(prolog_head(clause));
    lvars pred = predicate_record(fn, arity, true), proc;
    unless (pred_isdefined(pred) ->> proc) and is_dynamic(proc) then
        declare_dynamic(fn, arity);
        predicate_record(fn, arity, false) -> pred;
        pred_valof(pred) -> proc;
    endunless;
    fast_apply(comp_dynamic_predicate(clause, pred), proc, insert);
enddefine;

;;; prolog_asserta:
;;;     add a new clause to the start of a procedure.

define prolog_asserta =
    assert(% insert_at_front %);
enddefine;

;;; prolog_assertz:
;;;     add a new clause to the end of a procedure.

define prolog_assertz =
    assert(% insert_at_end %);
enddefine;

;;; prolog_assert:
;;;     add a new clause before clause number -posn- in a procedure.

define prolog_assert(clause, posn);
    lvars clause;
    dlvars posn;
    assert(clause,
        procedure(clause, proc);
            lvars clause, proc;
            insert_at_posn(clause, posn, proc);
        endprocedure);
enddefine;

/*
 *  Deleting clauses from the database (retract)
 */

;;; retract/1
;;;     retract clauses which unify with -clause-

define retract\/1(/* clause */) with_nargs 1;
    lvars clause = prolog_deref(/* clause */);
    if isprologvar(clause) then bad_goal(clause, "retract", 1) endif;
    lvars head = prolog_head(clause), body = prolog_body(clause);
    lvars (fn, arity) = prolog_termspec(head);
    lvars pred = predicate_record(fn, arity, true), proc;
    unless (pred_isdefined(pred) ->> proc) and is_dynamic(proc) then
        declare_dynamic(fn, arity);
        predicate_record(fn, arity, false) -> pred;
        pred_valof(pred) -> proc;
    endunless;
    SAVE;
    lvars tmp, clauses = is_dynamic(proc);
    until (Back(clauses) ->> tmp) == [] do
        prolog_instance(proc_clauses(Front(tmp))) -> clause;
        if prolog_unify(head, prolog_head(clause))
        and prolog_unify(body, prolog_body(clause))
        then
            delete_here(clauses, proc);
            prolog_apply_continuation();
            until Back(clauses) do
                Front(clauses) -> clauses;
            enduntil;
        else
            tmp -> clauses;
        endif;
        RESTORE;
    enduntil;
enddefine;

;;; prolog_remove:
;;;     delete a clause number -posn- from the predicate -fn/arity-

define prolog_remove(posn, fn, arity);
    lvars posn, fn, arity;
    lvars pred = predicate_record(fn, arity, true), proc;
    unless (pred_isdefined(pred) ->> proc) and is_dynamic(proc) then
        declare_dynamic(fn, arity);
        predicate_record(fn, arity, false) -> pred;
        pred_valof(pred) -> proc;
    endunless;
    delete_at_posn(posn, proc);
enddefine;

/*
 *  Retrieving clauses from the database
 */

;;; subscrln:
;;;     utility procedure which indexes a list, but returns nil for an
;;;     index out of range.

define lconstant subscrln(posn, list);
    lvars posn, list;
    if posn < 0 or posn > listlength(list) then
        [];
    else
        subscrl(intof(posn), list)
    endif
enddefine;

;;; prolog_raw_clause:
;;;     returns the clause number -posn- from a predicate -fn/arity-.
;;;     The clause is left "general"; i.e. with variable tokens in place of
;;;     real variables.
;;;     If the clause doesn't exist, nil is returned.

define prolog_raw_clause(posn, fn, arity);
    lvars posn, fn, arity;
    lvars pred = predicate_record(fn, arity, true), proc, clauselist;
    unless pred_isdefined(pred) ->> proc then
        [];
    elseunless is_dynamic(proc) ->> clauselist then
        subscrln(posn, proc_clauses(proc));
    elseunless isprocedure(subscrln(posn, Back(clauselist)) ->> proc) then
        [];
    else
        proc_clauses(proc);
    endunless;
enddefine;

;;; prolog_clause:
;;;     same as -prolog_raw_clause-, but with real variables in place of
;;;     tokens.

define prolog_clause =
    prolog_raw_clause <> prolog_instance;
enddefine;

;;; prolog_clauses:
;;;     returns all the clauses of predicate -fn/arity- in a dynamic list.

define prolog_clauses(fn, arity);
    lvars fn, arity;

    define lconstant generate_dynamic_clauses(clauseref);
        lvars clauseref, clauses, clause;
        Cont(clauseref) -> clauses;
        until Back(clauses) do
            Front(clauses) -> clauses;
        enduntil;
        if (Back(clauses) ->> clauses) == [] then
            termin;
        else
            clauses -> Cont(clauseref);
            prolog_instance(proc_clauses(Front(clauses)));
        endif;
    enddefine;

    define lconstant generate_static_clauses(clauseref);
        lvars clauseref, clauses;
        if (Cont(clauseref) ->> clauses) == [] then
            termin;
        else
            prolog_instance(Destpair(clauses) -> Cont(clauseref));
        endif;
    enddefine;

    lvars pred = predicate_record(fn, arity, true), proc, clauselist;
    returnunless(pred_isdefined(pred) ->> proc)([]);
    pdtolist(
        if is_dynamic(proc) ->> clauselist then
            generate_dynamic_clauses(% consref(clauselist) %);
        else
            generate_static_clauses(% consref(proc_clauses(proc)) %);
        endif);
enddefine;

;;; clause/2

define clause\/2(/* Head, */ Body) with_nargs 2;
    lvars clauses, clause, Head = prolog_deref(/* Head */), Body;
    if isprologvar(Head) then
        bad_goal(Head, Body, "clause", 2);
    endif;
    prolog_clauses(prolog_termspec(Head)) -> clauses;
    prolog_deref(Body) -> Body;
    SAVE;
    until null(clauses) do
        Destpair(clauses) -> (clause, clauses);
        if prolog_unify(Head, prolog_head(clause))
        and prolog_unify(Body, prolog_body(clause))
        then
            prolog_apply_continuation();
        endif;
        RESTORE;
    enduntil;
enddefine;

/*
 *  Querying the database
 */

;;; listpredicate:
;;;     lists the predicate with the specified name and arity.

define listpredicate(name, arity);
    lvars i = 1, c, name, arity;
    until (prolog_raw_clause(i, name, arity) ->> c) == nil do
        prolog_list(c);
        i fi_+ 1 -> i;
    enduntil;
    unless i == 1 then nl(1) endunless;
enddefine;

;;; listpredicates:
;;;     lists all predicates with the specified name.

define listpredicates(fn);
    lvars fn;
    app_named_predicates(fn, pred_spec <> listpredicate);
enddefine;

define listall();
    app_predicates(pred_spec <> listpredicate);
enddefine;

;;; makeempty:
;;;     create an instance of a term having functor -fn- and arity -arity-

define lconstant makeempty(fn, arity);
    lvars i, arity, fn;
    prolog_maketerm(for i to arity do prolog_newvar() endfor, fn, arity);
enddefine;

;;; current_atom/1
;;;     searches through the dictionary for all words

define current_atom\/1(/* Atom */) with_nargs 1;
    lvars Atom = prolog_deref(/* Atom */);

    ;;; test whether a word is in the dictionary
    define lconstant indictionary(word);
        lvars word;
        define lconstant cmp(w);
            lvars w;
            if w == word then exitfrom(true, indictionary) endif;
        enddefine;
        appdic(cmp);
        false;
    enddefine;

    define lconstant try_word(word);
        lvars word;
        SAVE;
        prolog_assign(Atom, if word == "'[]'" then [] else word endif);
        prolog_apply_continuation();
        RESTORE;
    enddefine;

    if isprologvar(Atom) then
        appdic(try_word);
    elseif Atom == [] or isword(Atom) and indictionary(Atom) then
        prolog_apply_continuation();
    endif;
enddefine;

;;; current_functor/2
;;;     searches the dictionary for all words of the form "fn/arity" and
;;;     creates the most general instance of the functor

define current_functor\/2(Fn, Term);
    lvars Fn, Term;

    define lconstant splitname(word);
        lvars l, p, word, arity;
        datalength(word) -> l;
        if l fi_> 2 and (locchar(`/`, 2, word) ->> p) and p fi_< l then
            substring(p fi_+ 1, l fi_- p, word) -> arity;
            if strnumber(arity) ->> arity then
                arity, consword(substring(1, p fi_- 1, word));
                return;
            endif;
        endif;
        false;
    enddefine;

    define lconstant try_word(word);
        lvars word, fn, arity;
        SAVE;
        if splitname(word) ->> fn then
            -> arity;
            if fn == "'[]'" then [] -> fn endif;
            if prolog_unify(Fn, fn)
            and prolog_unify(Term, makeempty(fn, arity))
            then
                prolog_apply_continuation();
            endif;
            RESTORE;
        endif;
    enddefine;

    appdic(try_word);
enddefine;

;;; current_predicate/2
;;;     searches for all predicates which are defined and have clauses
;;;     and creates the most general instance of the functor.

define current_predicate\/2(/* Fn, */ Term) with_nargs 2;
    lvars Fn = prolog_deref(/* Fn */), Term;

    define lconstant try_fn(pred);
        lvars pred;
        SAVE;
        if pred_has_clauses(pred) then
            if prolog_unify(Term, makeempty(pred_spec(pred))) then
                prolog_apply_continuation();
            endif;
            RESTORE;
        endif;
    enddefine;

    define lconstant try_pred(pred);
        lvars pred;
        SAVE;
        if pred_has_clauses(pred) then
            prolog_assign(Fn, pred_functor(pred));
            if prolog_unify(Term, makeempty(pred_spec(pred))) then
                prolog_apply_continuation();
            endif;
            RESTORE;
        endif;
    enddefine;

    if isword(Fn) or Fn == [] then
        app_named_predicates(Fn, try_fn);
    elseif isprologvar(Fn) then
        app_predicates(try_pred);
    endif;
enddefine;

endsection;     /* prolog */

PROLOG

:- module prolog.

asserta(Clause) :-
    var(Clause),
    !,
    bad_goal(asserta(Clause)).
asserta(Clause) :-
    prolog_eval(prolog_asserta(quote(Clause))).

assertz(Clause) :-
    var(Clause),
    !,
    bad_goal(assertz(Clause)).
assertz(Clause) :-
    prolog_eval(prolog_assertz(quote(Clause))).

assert(Clause) :-
    var(Clause),
    !,
    bad_goal(assert(Clause)).
assert(Clause) :-
    prolog_eval(prolog_assertz(quote(Clause))).


retractall(Clause) :-
    retract(Clause),
    fail.
retractall(Head) :-
    retract(Head :- Body),
    fail.
retractall(_).


prolog_abolish(Fn, Arity) :-
    predicate_spec(Fn/Arity, Fn, Arity),
    prolog_eval(predicate_abolish(quote(Fn, Arity), valof(true))).

abolish(Fn, Arity) :-
    predicate_spec(Fn/Arity, Fn, Arity),
    prolog_eval(predicate_abolish(quote(Fn, Arity), valof(false))).


listing :-
    prolog_eval(apply(valof(listall))).

listing(X) :-
    var(X),
    !,
    bad_goal(listing(X)).
listing([]) :-
    !.
listing([Pred|Preds]) :-
    !,
    listing(Pred),
    listing(Preds).
listing(Fn/Arity) :-
    atom(Fn),
    integer(Arity),
    !,
    prolog_eval(listpredicate(quote(Fn, Arity))).
listing(Pred) :-
    atom(Pred),
    !,
    prolog_eval(listpredicates(quote(Pred))).

:- endmodule prolog.


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 15 1993
        current_{atom,functor,predicate} definitions moved in from (defunct)
        "statepreds.p"; support for assert and retract moved in from
        "procedures.p". Abandoned the use of define-forms for predicates.
--- Robert John Duncan, Jun 24 1991
        Renamed define forms.
--- Rob Duncan, Aug  8 1989
    Sectionised and added #_INCLUDEs for POPC; moved in listing procedures
    from "write.p"; minor changes to inline assert, asserta, assertz.
--- Rob Duncan, Sep  5 1988
    Rewrote to use -prolog_pdr- and -prolog_inline_pdr-
    Added definition of 'prolog_abolish/2'.
 */
