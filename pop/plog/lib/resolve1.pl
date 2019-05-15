/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/plog/lib/resolve1.pl
 *  Purpose:        Simple resolution program MARK I.
 *  Author:         Unknown, ???
 *  Documentation:
 *  Related Files:
 */

/* Simple resolution program MARK I.

   Type:
        ?- resolve.

   to try and solve a problem. The negation of the theorem to be proved
   should be one of the FACTs.

   Clauses are expected to be in the format given in TEACH RESOLUTION
   Use the library file CLAUSIFY to convert expressions in Predicate
   Calculus format.
*/

;;; these operator declarations clash with library clausify ???
:- clause('DEC10', true) -> op(790, fx, [fact, #]); op(99, fx, [fact, #]).

:- library(gensym).

/* Doing the proof */

resolve :-
   number_facts,
   resolve1([]).

resolve1(Done) :-
   fact(L1,N1), fact(L2,N2),
   not(member([N1,N2],Done)),
   select(X,L1,L11),
   select(#X,L2,L22),
   append(L11,L22,NewL),
   !,
   conclude(NewL,[[N1,N2]|Done]).
resolve1(_) :-
   write('Sorry - cant prove that'), nl.

conclude([],_) :- !,
   write('Proof complete').
conclude(L,Done) :-
   addfact(L),
   resolve1(Done).

select(X,[X|L],L).
select(X,[Y|L],[Y|L1]) :- select(X,L,L1).

append([],X,X).
append([A|B],C,[A|D]) :- append(B,C,D).

/* Keeping track of numbers with facts */

number_facts :-
   retractall(fact(_,_)),
   fact(L),
   addfact(L), fail.
number_facts.

addfact(L) :-
   next_number(N),
   assertz(fact(L,N)).

next_number(X) :-
   retract(last_number(X1)), !,
   X is X1+1,
   asserta(last_number(X)).
next_number(1) :-
   asserta(last_number(1)).

member(X,[X|L]).
member(X,[Y|L]) :- member(X,L).
