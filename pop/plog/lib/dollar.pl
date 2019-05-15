/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/plog/lib/dollar.pl
 *  Purpose:        syntax for calling predicates whose names are in variables
 *  Author:         Jonathan Laventhol, JUn 28 1983
 *  Documentation:  HELP * DOLLAR
 *  Related Files:
 */

;;; nice syntax for calling predicates whose names are in variables
;;; two forms of use
;;;     ?- Pred $ <List of arguments> .
;;;     ?- Pred $ ( <comma separated arguments> ).
;;; the first form is quicker, and works with predicates of zero arity, and
;;; where one of the arguments is a term with comma as functor.

:- clause('DEC10', true) -> op(800, xfy, $); op(100, xfy, $).    

Functor$[] :-
    !,
    functor(X,Functor,0),
    call(X).
Functor$List :-
    List = [_|_],
    !,
    X =.. [Functor|List],
    call(X).
Functor$Arg :-
    listofcommas(Arg, Arglist),
    X =.. [Functor|Arglist],
    call(X).

listofcommas(Args, [A|C]) :-
    nonvar(Args),
    Args = (A,B), !,
    listofcommas(B, C).
listofcommas(A, [A]).
