/*  --- Copyright University of Sussex 1987. All rights reserved. ----------
 >  File:           $usepop/master/C.all/plog/lib/chatty.pl
 >  Purpose:        make prolog clauses give a commentary on their execution.
 >  Author:         Roger Evans, May 1983 (see revisions)
 >  Documentation:  HELP * CHATTY
 */

:- current_op(Prec, Fix, spy),
    op(Prec, Fix, chatty),
    op(Prec, Fix, unchatty).

/* chatty is called in the same way as spy. Argument is a term or a list of
   terms. Each term is of the form foo/3, meaning predicate foo with 3
   arguments.
*/

(chatty []).
(chatty [A|B]) :- dochatty(A),(chatty B).
(chatty A) :- dochatty(A).

(unchatty []).
(unchatty [A|B]) :- undochatty(A), (unchatty B).
(unchatty A) :- undochatty(A).

/* clear all chattiness */
unchattyall :- retract(ischatty(F/N)),undoclauses(F,N),fail.
unchattyall.

ischatty(X) :- fail.

/* ------------------------------------------------------------------------ */

/* set C to chatty, if not done already - doclauses modifies existing
   clauses, then we add new ones to the top and bottom */

dochatty(C) :- C = F/N,!,
               functor(A,F,N),
               (    ischatty(C),
                    write(C),write(' is already chatty.'),nl
                    ;
                    asserta(ischatty(C)),
                    doclauses(F,N,1),
                    asserta(A :- ch_start(A)),
                    assertz(A :- (ch_nomore(A),!,fail)),
                    write(C),write(' is now chatty.'),nl
                ),!.
;;; next clause added to allow chatty foo, which will make chatty all foo
;;; predicates of all arities.  jonathan laventhol.
dochatty(C) :-
    prolog_eval(prolog_predicates_defined(C), L), L \= [], !,
    dochattylist(C, L).
dochatty(A) :- chattymishap(A).

;;; for functor and list of arities
dochattylist(C, []) :- !.
dochattylist(C, [H|T]) :-
    dochatty(C/H), dochattylist(C, T).


/* undochatty - clear chattiness if necessary, using undoclauses */

undochatty(C) :- C = F/N,!,
                 functor(A,F,N),
                 (  retract(ischatty(C)),
                    undoclauses(F,N), write(C),write(' is no longer chatty.'),nl
                    ;
                    write(C),write(' is not chatty.'),nl
                 ),!.
;;; next clause added to allow chatty foo, which will make unchatty all foo
;;; predicates of all arities.
undochatty(C) :-
    prolog_eval(prolog_predicates_defined(C), L), L \= [], !,
    undochattylist(C, L).
undochatty(A) :- chattymishap(A).

;;; for functor and list of arities
undochattylist(C, []) :- !.
undochattylist(C, [H|T]) :-
    undochatty(C/H), dochattylist(C, T).

chattymishap(A) :-
   write('Can\'t do:  '),write(A),nl.                    

/* ------------------------------------------------------------------------ */

/* clause rewriting stuff */

/* retract top clause of pred, do all the other clauses then assert
   modified clauses at top (in chattycall) */
doclauses(F,N,Ctr) :-
    functor(A,F,N),
    retract(A :- B),!,
    Ctr2 is Ctr+1,
    doclauses(F,N,Ctr2),!,
    chattycall(A,B,Ctr,F).
/* no more clauses - do nothing */
doclauses(F,N,Ctr).



/* retract top clause, undo all the rest, then if top clause was a chatty
   clause, put back old version, otherwise ignore it */
undoclauses(F,N) :-
    functor(A,F,N),
    retract(A :- B),!,
    undoclauses(F,N),!,
    (B = (ch_try(_,_),B1,_;_),asserta(A :- B1); true),!.
undoclauses(F,N).

/* for each original clause set up a new one which prints appropriate
   messages  */
chattycall(A,B,Ctr,F) :-
    asserta(( A :- ( ch_try(Ctr,F),
                     B,
                     ch_succeed(Ctr,F)
                     ;
                     ch_fail(Ctr,F),fail )
           )).

/* ------------------------------------------------------------------------ */

/* indentation handling */

:- prolog_language(pop11).

/* set prolog_toplevel_trap to reset chatty indent */

vars prolog_toplevel_trap chatty_indent;

define chatty_toplevel_trap(prolog_toplevel_trap);
    0 -> chatty_indent;
    prolog_toplevel_trap();
enddefine;

define chattyup; chatty_indent+1 -> chatty_indent; enddefine;
define chattydown; max(chatty_indent-1,0) -> chatty_indent; enddefine;                                         


unless isclosure(prolog_toplevel_trap) and
       pdpart(prolog_toplevel_trap) == chatty_toplevel_trap
then
    chatty_toplevel_trap(%prolog_toplevel_trap%) -> prolog_toplevel_trap;
endunless;

:- prolog_language(prolog).


chattymark :- N is valof(chatty_indent),write(-),chdomark(N),!.

    chdomark(0) :- !.
    chdomark(N) :- write(' |'),N1 is N - 1, chdomark(N1).

chattyset. /* just in case its in anyone's program! */

chattyup :- prolog_eval(apply(valof(chattyup))).
chattydown :- prolog_eval(apply(valof(chattydown))).

/* ------------------------------------------------------------------------ */

/* chatty messages */

ch_start(A) :- chattymark, chattyup, write(' Starting '), write(A), nl,fail.

ch_try(Ctr,F) :- chattymark,write(' Try'),sayclause(Ctr,F),nl,!.

ch_succeed(Ctr,F) :-
    chattymark,chattydown,sayclause(Ctr,F),write(' succeeded.'),nl
    ;
    chattyup,chattymark,write(' Retry'),sayclause(Ctr,F),nl,fail.

ch_fail(Ctr,F) :- chattymark, sayclause(Ctr,F),write(' failed.'),nl,!.

ch_nomore(A) :-
        chattydown,chattymark,
        write(' No more clauses for '),write(A),nl,!.

sayclause(C,F):- write(' clause '),write(C),write(' of '),write(F),!.                    

/*  --- Revision History ---------------------------------------------------
--- Simon Nichols, Dec 14 1987 - Changed precedence and fixity of chatty/1
    and unchatty/1 to be the same as those of spy/1.
--- Roger Evans, Mar 13 1984 - Modified to make use of prolog_toplevel_trap
    to reset indentation - chattyset now redundant.
--- Jonathan Laventhol, Mar 12 1984 - minor additions made to allow the
    dropping of the slash-arity.
 */
