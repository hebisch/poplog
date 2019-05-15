/*  --- Copyright University of Sussex 1990. All rights reserved. ----------
 >  File:           C.all/plog/lib/useful.pl
 >  Purpose:        Library defining some useful predicates
 >  Author:         Unknown (see revisions)
 >  Documentation:  PLOGHELP * USEFUL
 */

:- global
    append/3,
    load/1,
    member/2,
    once/1,
    uses/1.


;;; append(List1, List2, List3) :-
;;;     succeeds whenever List3 is the concatenation of List1 and List2

append([], List, List).
append([X|List1], List2, [X|List3]) :-
    append(List1, List2, List3).


;;; load(File) :-
;;;     reconsults File

load(File) :-
    reconsult(File).


;;; member(Item, List) :-
;;;     succeeds whenever Item is a member of List

member(Item, [Item|_]).
member(Item, [_|List]) :-
    member(Item, List).


;;; once(Goal) :-
;;;     calls Goal without backtracking

once(Goal) :-
    call(Goal), !.


;;; uses(Pred) :-
;;;     loads the library file 'Pred.pl' only if there is no existing
;;;     definition of Pred

uses(Pred) :-
    current_predicate(Pred, _), !.
uses(Pred) :-
    library(Pred).


/* --- Revision History ---------------------------------------------------
--- Rob Duncan, Apr  9 1990
        Made definitions global.
 */
