/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/grammar.p
 > Purpose:         Prolog: grammar rule expansion
 > Author:          Jonathan Laventhol, 1985 (see revisions)
 */

PROLOG

:- op(255, xfx, [-->]).

:- module prolog.

phrase(T, S) :-
    phrase(T, S, []).

phrase(T, S0, S) :-
    prolog_tag(T, S0, S, G),
    call(G).

prolog_grexpand(P0, Q0, P, Q) :-
    prolog_dcglhs(P0, S0, S, P),
    prolog_dcgrhs(Q0, S0, S, Q1),
    prolog_flatconj(Q1, Q).

prolog_dcglhs((NT,Ts), S0, S, P) :-
    !,
    nonvar(NT),
    prolog_islist(Ts),
    prolog_tag(NT, S0, S1, P),
    append(Ts, S, S1).
prolog_dcglhs(NT, S0, S, P) :-
    nonvar(NT),
    prolog_tag(NT, S0, S, P).

prolog_dcgrhs(X, S0, S, phrase(X,S0,S)) :-
    var(X),
    !.
prolog_dcgrhs((X1,X2), S0, S, P) :-
    !,
    prolog_dcgrhs(X1, S0, S1, P1),
    prolog_dcgrhs(X2, S1, S, P2),
    prolog_dcgand(P1, P2, P).
prolog_dcgrhs((X1;X2), S0, S, (P1;P2)) :-
    !,
    prolog_dcgor(X1, S0, S, P1),
    prolog_dcgor(X2, S0, S, P2).
prolog_dcgrhs({P}, S, S, P) :-
    !.
prolog_dcgrhs(!, S, S, !) :-
    !.
prolog_dcgrhs(Ts, S0, S, true) :-
    prolog_islist(Ts),
    !,
    append(Ts, S, S0).
prolog_dcgrhs(X, S0, S, P) :-
    prolog_tag(X, S0, S, P).

prolog_dcgand(true, P, P) :-
    !.
prolog_dcgand(P, true, P) :-
    !.
prolog_dcgand(P, Q, (P,Q)).

prolog_dcgor(X, S0, S, P) :-
    prolog_dcgrhs(X, S0a, S, Pa),
    (var(S0a), S0a \== S, !,
        S0 = S0a,
        P = Pa
    ;
        P = (S0=S0a, Pa)
    ).

prolog_flatconj(A, A) :-
    var(A),
    !.
prolog_flatconj((A,B), C) :-
    !,
    prolog_flatconj(A, C, R),
    prolog_flatconj(B, R).
prolog_flatconj(A, A).

prolog_flatconj(A, (A,R), R) :-
    var(A),
    !.
prolog_flatconj((A,B), C, R) :- !,
    prolog_flatconj(A, C, R1),
    prolog_flatconj(B, R1, R).
prolog_flatconj(A, (A,R), R).

prolog_tag(X, S0, S, Y) :-
    X =.. XL,
    append(XL, [S0, S], YL),
    Y =.. YL.

prolog_islist([]) :-
    !.
prolog_islist([_|_]).

:- endmodule prolog.


/* --- Revision History ---------------------------------------------------
--- Simon Nichols, Jul 19 1990
        Removed expand_term/2. This is now defined in "systempreds.p" to
        perform general clause pre-processing.
--- Rob Duncan, Aug  8 1989
        Sectionised and added #_INCLUDEs for POPC.
--- Rob Duncan, Mar 10 1989
        Fixed to allow for meta-calls on the RHS of grammar rules: added
        an extra first clause to -prolog_dcgrhs- to spot these. Meta-call
        expands to a call to -phrase/3-, so this has to be exported from
        section prolog in "prolog.p".
        Tidied up.
--- Rob Duncan, Mar 16 1988
        Renamed from ploggram.p
 */
