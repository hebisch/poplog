/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/plog/lib/higher_order.pl
 > Purpose:         Defines some higher-order predicates.
 > Author:          Simon Nichols, May 21 1992
 > Documentation:   PLOGHELP * HIGHER_ORDER
 */


% maplist(+Pred, +Xs)
%   is true when Pred(X) is true for each X in Xs.

maplist(P, []).
maplist(P, [X|Xs]) :-
    call(P, X),
    maplist(P, Xs).


% maplist(+Pred, +Xs, ?Ys)
%   is true when Pred(X, Y) is true for each corresponding X in Xs and Y in Ys.

maplist(P, [], []).
maplist(P, [X|Xs], [Y|Ys]) :-
    call(P, X, Y),
    maplist(P, Xs, Ys).


% reduce(+Pred, +Xs, -Answer)
%   is true when ???

reduce(P, [X|Xs], Ans) :-
    reduce_1(Xs, P, X, Ans).

reduce_1([], P, Ans, Ans).
reduce_1([X|Xs], P, Ans0, Ans) :-
    call(P, Ans0, X, Ans1),
    reduce_1(Xs, P, Ans1, Ans).


% mapargs(+Pred, +Term)
%   is true when Pred(Arg) is true for each argument Arg of Term.

mapargs(P, Term) :-
    mapargs_1(1, P, Term).

mapargs_1(I, P, Term) :-
    arg(I, Term, Arg), !,   % red cut
    call(P, Arg),
    I1 is I + 1,
    mapargs_1(I1, P, Term).
mapargs_1(_, _, _).


% mapargs(+Pred, +Term1, ?Term2)
%   is true when Pred(Arg1, Arg2) is true for each corresponding argument Arg1
%   of Term1 and each argument Arg2 of Term2.

mapargs(P, Term1, Term2) :-
    functor(Term1, F, A),
    functor(Term2, F, A),
    mapargs_1(1, P, Term1, Term2).

mapargs_1(I, P, Term1, Term2) :-
    arg(I, Term1, Arg1), !,     % red cut
    arg(I, Term2, Arg2),
    call(P, Arg1, Arg2),
    I1 is I + 1,
    mapargs_1(I1, P, Term1, Term2).
mapargs_1(_, _, _, _).
