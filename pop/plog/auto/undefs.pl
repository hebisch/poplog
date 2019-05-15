/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/plog/auto/undefs.pl
 *  Purpose:        package for printing or abolishing all dummy undef preds
 *  Author:         Jonathan Laventhol, Sep  6 1983
 *  Documentation:  HELP * UNDEFS
 *  Related Files:
 */

;;; package for printing out or abolishing all the predicates with the dummy
;;;     foo :- fail, 'UNDEFINED-PREDICATE'.
;;; definition given when called without a definition.
;;;
;;; the pop11 procedure list_of_prednames returns a list of all the predicates
;;; which are defined
;;;     [ [foo 1] [foo 2] ]
;;; means foo/1 foo/2.  note that this list may include predicates which
;;; are no longer in existence -- but all the existing predicates will be
;;; in this list (due to funny internal workings of prolog).
;;; the rest of the prolog code should be self-explanatory.

:- prolog_language("pop11").

define list_of_prednames();
    [% prolog_apppredicates_defined(procedure(name, arity);
                                        [^name ^arity]
                                    endprocedure) %]
enddefine;

:- prolog_language("prolog").

/* undef_predlist(List_of_preds, List_of_undef_preds)   */
/* prunes list of preds into list of undefined preds    */

/* found an undefined one */
undef_predlist([[Name, Arity] | T], [[Name,Arity] | Undefs]) :-
    functor(Func, Name, Arity),
    clause(Func, (fail, 'UNDEFINED-PREDICATE')), !,
    undef_predlist(T, Undefs).
/* found a defined one */
undef_predlist([_|T], U) :-
    undef_predlist(T, U), !.
/* end of list */
undef_predlist([], []).

/* returns a list of undefined predicate name/arity lists */
undef_predlist(U) :-
    prolog_eval(apply(valof(list_of_prednames)), L),
    undef_predlist(L, U).

/* prints name/arity for every undefined predicate  */
undefs :-
    undef_predlist(L), L \= [], !,
    write('Undefined predicates:'), nl,
    write_list(L).
/* unless there aren't any */
undefs :-
    write('No undefined predicates'), nl, fail.

/* prints a list of two element lists nicely */
write_list([[N,A]|T]) :-
    tab(4), write(N), write('/'), write(A), nl,
    write_list(T).
write_list([]).

/* abolishes all the undefined predicates   */
abundefs :-
    undef_predlist(L), L \= [], !,
    write('Abolishing undefined predicates:'), nl,
    abundefs(L).
/* unless there aren't any */
abundefs :-
    write('No undefined predicates to abolish').

/* abolishes a list of name/arity lists */
abundefs([[N,A]|T]) :-
    abolish(N, A), tab(4), write(N), write('/'), write(A), nl,
    abundefs(T).
abundefs([]).
