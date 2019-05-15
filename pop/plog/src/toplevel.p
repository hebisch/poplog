/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:        C.all/plog/src/toplevel.p
 > Purpose:     Prolog: top-level loop
 > Author:      Jonathan Laventhol, 1985(?) (see revisions)
 */

PROLOG

:- module prolog.

break :-
    seeing(OldInput), telling(OldOutput),
    tell(user), format("[break]~2n"),
    compile(user, query),
    tell(user), format("[end break]~n"),
    see(OldInput), tell(OldOutput).

prolog_toplevel(Interm, Vars) :-
    prolog_goal(Interm,Vars).

prolog_goal(Var,_) :-               ;;; uninstantiated variable - error
    var(Var), !,
    call(Var).
prolog_goal(:- Cmd, _) :- !,        ;;; directive
    prolog_directive(Cmd),
    nl.
prolog_goal(?- Query, Env) :- !,    ;;; query
    prolog_goal(Query, Env).
prolog_goal(Goal, Env) :-           ;;; succeed
    call(Goal),
    prolog_display_answer(Env),
    telling(OldFile), tell(user),
    format("yes~2n"),
    tell(OldFile).
prolog_goal(_,_) :-                 ;;; fail
    telling(OldFile), tell(user),
    format("no~2n"),
    tell(OldFile).

prolog_directive(Goal) :-
    call(Goal), !.
prolog_directive(Goal) :-
    telling(OldFile), tell(user),
    format("? ~w.~n", [Goal]),
    tell(OldFile).

prolog_display_answer(Vars) :-
     prolog_namedvars(Vars, Named),
     Named = [_|_], !,
     seeing(S), telling(T), see(user), tell(user),
     prolog_writevars(Named), tab,
     prolog_eval(prolog_readline('? '), Resp),
     prolog_writevarsnl(Named),
     see(S), tell(T),
     Resp = [].
prolog_display_answer(_).


;;; prolog_namedvars/2:
;;;     given an environment which maps variable names to values, returns a
;;;     new environment with anonymous variables removed.

prolog_namedvars([('_' = _)|Vs], T) :-
    !,
    prolog_namedvars(Vs, T).
prolog_namedvars([V|Vs], [V|T]) :-
    prolog_namedvars(Vs, T).
prolog_namedvars([], []).

;;; prolog_writevarsnl/1:
;;;     prints a newline if there is more than one named variable.

prolog_writevarsnl([_,_|_]) :-
    !, nl.
prolog_writevarsnl(_).

prolog_writevars([]).
prolog_writevars([T]) :-
    !,
    prolog_writevar(T).
prolog_writevars([T|Rest]) :-
    prolog_writevars(Rest), nl,
    prolog_writevar(T).

prolog_writevar(Name=Value) :-
    format("~w = ~p", [Name, Value]).

:- endmodule prolog.


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 15 1993
        Changed break/1 to call the compiler directly in query mode.
        Removed redundant calls to prolog_s*tate.
--- Robert John Duncan, Apr  9 1992
        Introduced format/[1,2]
--- Andrew Casson, May  3 1990
        Removed old 'prolog_namedvars/2' definition which was never called
        and renamed 'prolog_namedvars1/2' to 'prolog_namedvars/2'.
--- Rob Duncan, Aug  8 1989
        Sectionised and added #_INCLUDEs for POPC.
        Changed 'prolog_writevar/1' to expect new environment format.
--- Rob Duncan, Mar 16 1988
        Renamed from plogtop.p and tidied up
 */
