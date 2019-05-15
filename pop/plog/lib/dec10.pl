/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/plog/lib/dec10.pl
 >  Purpose:        DEC10 -- compatibility predicates
 >  Author:         Jonathan Laventhol, Mar 26 1984 (see revisions)
 >  Documentation:  HELP * DEC10
 >  Related Files:  C.all/plog/lib/dec10_ops.pl
 */

'DEC10'.        ;;; so we know

/* SOME THINGS IN LIBRARIES */
    :- library(dec10_ops).  ;;; DEC-10 operator precedences
    :- library(log).        ;;; terminal session logging
    :- library(record).     ;;; database stuff
    :- library(unknown).    ;;; action on unknown predicates

notimplemented(Term) :-
    functor(Term, Name, Arity),
    prolog_system_predicate(Name, Arity),
    write(Name), write('/'), write(Arity),
    write(' is now built into the system.\n').
notimplemented(Term) :-
    functor(Term, Name, Arity),
    current_predicate(Name, Term),
    write(Name), write('/'), write(Arity),
    write(' you already have a definition for, nothing done.\n').
notimplemented(Term) :-
    functor(Term, Name, Arity),
    assertz((
        Term :-
            error('Predicate not implemented', [Term])
        )).

;;; Dummy definitions for some predicates
;;; Some just succeed, or fail.
;;; Others give an error message

/* MODE DECLARATIONS */
    mode X.         ;;; no mode declarations
    public X.       ;;; only have a compiler, this is not meaningful

/* COMPILATION FLAGS */
    fastcode.       ;;; we try!
    compactcode.    ;;; and we try.

/* UTILITY */
    'C'([X|Y], X, Y).   ;;; our grammar rules don't use this

/* ANCESTORS and allied predicates */
    :- notimplemented(ancestors(X)).
    :- notimplemented(subgoal_of(A)).
    :- notimplemented(depth(A)).
    :- notimplemented(maxdepth(A)).
    revive(A,B).

/* DATABASE */
    :- notimplemented(assert(C,R)).
    :- notimplemented(asserta(C,R)).
    :- notimplemented(assertz(C,R)).
    :- notimplemented(clause(A,B,C)).

/* FILE HANDLING */
    fileerrors.     ;;; this always happens
    :- notimplemented(nofileerrors).

/* GC tuning */
    gc.
    nogc.
    :- notimplemented(gcguide(A,B,C)).

/* DEBUGGING FACILITIES */
    :- notimplemented(debug).
    :- notimplemented(trace).

/* SYSTEM STUFF */
    :- notimplemented(plsys(A)).

:- abolish(notimplemented, 1).


/* --- Revision History ---------------------------------------------------
--- Simon Nichols, Feb 11 1992
        Removed dummy definition for leash/1 as there is now a sensible one
        in the Prolog system.
--- Simon Nichols, Oct  8 1991
        Removed dummy definition for close/1 as there is now a sensible one
        in the Prolog system.
--- Simon Nichols, Mar 27 1990
        Moved out operator declarations to new library file DEC10_OPS.
--- Rob Duncan, Aug 21 1989
        Added operator declarations for modules.
--- Rob Duncan, Sep  8 1988
        Added operator declarations for 'dynamic', 'static' etc.
 */
