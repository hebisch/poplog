/*  --- Copyright University of Sussex 1993. All rights reserved. ----------
 >  File:           C.all/plog/lib/ctlib.pl
 >  Purpose:        library routines for Prolog Computers and Thought course
 >  Author:         Roger Evans, ??? (see revisions)
 >  Documentation:
 >  Related Files:
 */

/* load lib useful if not loaded */
:- clause(member(X,Y),Z); library(useful).

/* printer for words or strings */
pr([H | T]) :- !, put(H), !, pr(T), !.
pr([]) :- !.
pr(X) :- !, write(X), !.

/* space predicates */
sp :- put(32),!.
sp(N) :- !, tab(N),!.

/* mishap - args are just a message or a message and a list of culprits */
mishap(Message) :-
    nl, pr("MISHAP: "), pr(Message), nl, !, fail.
mishap(Message, List) :-
    nl, pr("MISHAP: "), pr(Message), nl,
    pr("Involving: "), pr_culprits(List), nl,
    !, fail.
pr_culprits([]).
pr_culprits([H | T]) :-
    pr(H), sp, pr_culprits(T), !.


/* readline predicate */
readline(L) :- prolog_eval(prolog_readline('? '), L), !.

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 14 1993
        Replaced prolog_safer*eadline with prolog_readline
 */
