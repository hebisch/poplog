/*  --- Copyright University of Sussex 1991. All rights reserved. ----------
 *  File:           C.all/plog/lib/planet.pl
 *  Purpose:        prolog half of a simple problem solver
 *  Author:         Roger Evans, November 1982 (see revisions)
 *  Documentation:  HELP * PLANET
 *  Related Files:  POP11 LIB * PLANET.P
 */

achieve(input(X),[],_) :- !,nonvar(X).
achieve(output(X),[],_) :- !,var(X).
achieve(gain(X),[],_) :- !,gainlist([X]).
achieve(lose(X),[],_) :- !,loselist([X]).
achieve(fact(X),[],_) :- !,clause(X,Y),call(X).
achieve(Goal,[],Done) :- member(Goal,Done),!,fail.
achieve(Goal,[proc(Proc,Procargs)|Plist],Done) :-
        'module'(Goal,Prelist,Gains,Losses,Proc,Procargs),
        achievelist(Prelist,Plist,[Goal|Done]),
        gainlist(Gains),
        loselist(Losses).

achievelist([],[],_) :- !.
achievelist([G|GL],[P|PL],D) :- !,
        achieve(G,P,D),
        achievelist(GL,PL,D).


gainlist(L) :- assertlist(L).
gainlist(L) :- retractlist(L),!,fail.

loselist(L) :- retractlist(L).
loselist(L) :- assertlist(L),!,fail.

assertlist([]) :- !.
assertlist([H|T]) :- !,asserta(H),!,assertlist(T),!.

retractlist([]) :- !.
retractlist([H|T]) :- retract(H),!,retractlist(T),!.

member(X,[X|_]).
member(X,[_|Y]) :- member(X,Y).

/*  --- Revision History ---------------------------------------------------
--- Simon Nichols, Mar 22 1991
        Put quotes around the atom 'module', which is an operator
        (modules are now built into Prolog).
--- Mark Rubinstein, Aug  9 1985 moved into the PROLOG library.
 */
