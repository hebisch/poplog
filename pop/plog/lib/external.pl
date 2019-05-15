/*  --- University of Sussex POPLOG file -----------------------------------
 > File:           $usepop/master/C.all/plog/lib/external.pl
 > Purpose:        Prolog interface to external_load and external_apply
 > Author:         Jonathan Laventhol & John Gibson, Dec 1984
 > Documentation:  REF * EXTERNAL
 > Related Files:
*/

append([], X, X) :- !.
append([A|B], X, [A|Z]) :-
    append(B, X, Z).

external_load(A,B,C) :-
    prolog_eval(external_load(A,B,C)).

external_call(T, RV) :-
    T =.. [Name|Args],
    length(Args, L),
    append(Args, [L, valof(true), valof(Name)], RA),
    RT =.. [external_apply|RA],
    prolog_eval(RT, RV).
external_call(T) :-
    T =.. [Name|Args],
    length(Args, L),
    append(Args, [L, valof(false), valof(Name)], RA),
    RT =.. [external_apply|RA],
    prolog_eval(RT).

:- prolog_language(pop11).

define pop_string(x);
    lvars x;
    x >< ''
enddefine;
