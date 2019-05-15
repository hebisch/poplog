/*  --- Copyright University of Sussex 1991. All rights reserved. ----------
 *  File:           C.all/plog/lib/strings.pl
 *  Purpose:        A package of predicates to make string output easier.
 *  Author:         Jonathan Laventhol, July  4 1983 (see revisions)
 *  Documentation:  HELP * STRINGS
 *  Related Files:
 */

;;;     ?- writes(List).    ;;; write an object, or string as characters
;;;     ?- putchar(Char).   ;;; put a character or single character string
;;; -------------------------------------------------------------------------

charcode(X) :-
    integer(X),
    0 =< X, X =< 255.

string(X) :-
    var(X),
    !, fail.
string([]) :- !.
string([Char|Rest]) :-
    charcode(Char),
    string(Rest).

writes(S) :-
    string(S), !,
    prolog_eval(applist(S, valof(cucharout))).
writes(X) :-
    write(X).

putchar([N]) :-
    charcode(N), !,
    put(N).
putchar(N) :-
    put(N).

/* --- Revision History ---------------------------------------------------
--- Simon Nichols, Jan 25 1991
        Changed string/1 to fail if its argument is a variable (rather than
        instantiating it). Added a cut to the second clause of string/1.
 */
