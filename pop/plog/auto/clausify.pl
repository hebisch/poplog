/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/plog/auto/clausify.pl
 *  Purpose:        Putting Predicate Calculus Formulae into Clausal Form
 *  Author:         Unknown, ???
 *  Documentation:  TEACH * RESOLUTION
 *  Related Files:
 */

/* Adapted from Clocksin and Mellish Appendix B */
/* Clauses are expressed in the notation of TEACH RESOLUTION */

:- library(gensym).

/* Use the following notation for Predicate Calculus formulae:   */
:- clause('DEC10', true) ->
    (op(450, fx, #),
     op(800, xfy, v),
     op(800, xfy, ^),
     op(850, xfy, ->),
     op(850, xfy, <->),
     op(880, fx, fact));

    (op(30, fx, #),                                 /* Not */
     op(100, xfy, v),                               /* Or  */
     op(100, xfy, ^),                               /* And */
     op(150, xfy, ->),                              /* Implies */
     op(150, xfy, <->),                             /* Equivalent */
     op(200, fx, fact)).                            /* For denoting clauses */

/* In addition:

   all(X,P)     denotes "for all X, P holds"
   exists(X,P)  denotes "there is an X such that P holds"

   For both of these X must be a PROLOG VARIABLE
   (note that this diverges from what Clocksin & Mellishs program requires)
*/

/* The main procedure */

clausify(X,Y) :-
   implout(X,X1),
   negin(X1,X2),
   skolem(X2,X3,[]),
   univout(X3,X4),
   conjn(X4,X5),
   toclauses(X5,Y,[]).

/* Removing Implications */

implout((P <-> Q),((P11 ^ Q11) v (#P1 ^ #Q1))) :- !,
   implout(P,P11), rename_vars(P11,P1,[]), implout(Q,Q11),
   rename_vars(Q11,Q1,[]).
implout((P -> Q),(#P1 v Q1)) :- !,
   implout(P,P1), implout(Q,Q1).
implout(all(X,P),all(X,P1)) :- !,
   implout(P,P1).
implout(exists(X,P),exists(X,P1)) :- !,
   implout(P,P1).
implout((P ^ Q),(P1 ^ Q1)) :- !,
   implout(P,P1), implout(Q,Q1).
implout((P v Q),(P1 v Q1)) :- !,
   implout(P,P1), implout(Q,Q1).
implout(#P,#P1) :- !,
   implout(P,P1).
implout(P,P).

/* Moving negation inwards */

negin(#P,P1) :- !, neg(P,P1).
negin(all(X,P),all(X,P1)) :- !, negin(P,P1).
negin(exists(X,P), exists(X,P1)) :- !, negin(P,P1).
negin((P ^ Q),(P1 ^ Q1)) :- !, negin(P,P1), negin(Q,Q1).
negin((P v Q),(P1 v Q1)) :- !, negin(P,P1), negin(Q,Q1).
negin(P,P).

neg(#P,P1) :- !, negin(P,P1).
neg(all(X,P),exists(X,P1)) :- !, neg(P,P1).
neg(exists(X,P),all(X,P1)) :- !, neg(P,P1).
neg((P ^ Q),(P1 v Q1)) :- !, neg(P,P1), neg(Q,Q1).
neg((P v Q),(P1 ^ Q1)) :- !, neg(P,P1), neg(Q,Q1).
neg(P,#P).

/* Skolemising */

skolem(all(X,P),all(X,P1),Vars) :- !,
   var(X),
   skolem(P,P1,[X|Vars]).
skolem(exists(X,P),P1,Vars) :- !,
   var(X),
   gensym(f,F),
   X =.. [F|Vars],
   skolem(P,P1,Vars).
skolem((P ^ Q),(P1 ^ Q1),Vars) :- !,
   skolem(P,P1,Vars), skolem(Q,Q1,Vars).
skolem((P v Q),(P1 v Q1),Vars) :- !,
   skolem(P,P1,Vars), skolem(Q,Q1,Vars).
skolem(P,P,_).

/* Moving universal quantifiers outwards */

univout(all(X,P),P1) :- !, univout(P,P1).
univout((P ^ Q),(P1 ^ Q1)) :- !,
   univout(P,P1), univout(Q,Q1).
univout((P v Q),(P1 v Q1)) :- !,
   univout(P,P1), univout(Q,Q1).
univout(P,P).

/* Distributing and over or */

conjn((P v Q),R) :- !,
   conjn(P,P1), conjn(Q,Q1),
   conjn1((P1 v Q1),R).
conjn((P ^ Q),(P1 ^ Q1)) :- !,
   conjn(P,P1), conjn(Q,Q1).
conjn(P,P).

conjn1(((P ^ Q) v R),(P1 ^ Q1)) :- !,
   conjn((P v R),P1), conjn((Q v R),Q1).
conjn1((P v (Q ^ R)),(P1 ^ Q1)) :- !,
/*   conjn((P v Q),P1), conjn((P1 v R),Q1). */
   conjn((P v Q),P1), conjn((P v R),Q1).
conjn1(P,P).

/* Putting into clauses */

toclauses((P ^ Q),C1,C2) :- !,
   toclauses(P,C1,C3), toclauses(Q,C3,C2).
toclauses(P,[fact(C)|Cs],Cs) :- inclause(P,C,[]), !.
toclauses(P,C,C).

inclause((P v Q),A,A1) :- !,
   inclause(P,A2,A1), inclause(Q,A,A2).
inclause(#P,A1,A) :- !,
   notin(P,A), putin(#P,A,A1).
inclause(P,A1,A) :- !,
   notin(#P,A), putin(P,A,A1).

notin(X,[Y|_]) :- X == Y, !, fail.
notin(X,[_|L]) :- !, notin(X,L).
notin(_,[]).

putin(X,[],[X]) :- !.
putin(X,[Y|L],L) :- X == Y, !.
putin(X,[Y|L],[Y|L1]) :- putin(X,L,L1).

;;; Renaming variables, when a quantified proposition is split into
;;; two. This avoids nasty problems where the same variable is both
;;; existentially and universally quantified over.

rename_vars(X,Y,Assocs) :- var(X), !, assoc(X,Y,Assocs).
rename_vars(X,X,_) :- atomic(X), !.
rename_vars(all(X,Y),all(X1,Y1),Ass) :- !,
   rename_vars(Y,Y1,[[X|X1]|Ass]).
rename_vars(exists(X,Y),exists(X1,Y1),Ass) :- !,
   rename_vars(Y,Y1,[[X|X1]|Ass]).
rename_vars(X,Y,Ass) :-
   functor(X,F,N), functor(Y,F,N),
   rename_vars_args(N,X,Y,Ass).

rename_vars_args(0,_,_,_) :- !.
rename_vars_args(N,X,Y,Ass) :-
   arg(N,X,X1), arg(N,Y,Y1),
   rename_vars(X1,Y1,Ass),
   N1 is N - 1, rename_vars_args(N1,X,Y,Ass).

assoc(X,Y,[]) :- !, X=Y.
assoc(X,Y,[[X1|Y1]|_]) :- X == X1, !, Y=Y1.
assoc(X,Y,[_|L]) :- assoc(X,Y,L).
