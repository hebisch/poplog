/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/spy.p
 > Purpose:         Prolog: the spy debugger
 > Author:          Rob Duncan and Simon Nichols, Nov. 20 1987 (see revisions)
 */

section prolog;

constant
    procedure ( predicate_valof, unwrapped, pred_spec, app_predicates,
        pred_raw_valof, add_wrapper, app_named_predicates, predicate_record,
        pred_isdefined, del_wrapper, predicate_has_clauses, prolog_print,
        prolog_display, );

;;; ========================================================================

vars
    oride = "no",
        ;;; Set to "yes" when spying is disabled
    push = "any",
        ;;; Port to go to
    invoc = 0,
        ;;; Invocation number
    skip = 0,
        ;;; Invocation to skip to
    prolog_quiet_spy = false,
        ;;; true when spying messages are suppressed (used from Prolog)
;

lvars
    spied = false,
        ;;; true if spypoints have been set or unset
;

;;; spymessage:
;;;     like -printf- but does nothing if -prolog_quiet_spy- is true

define lconstant spymessage();
    dlocal cucharout, pr = syspr;
    if prolog_quiet_spy then erase -> cucharout endif;
    printf();
enddefine;

;;; spy_action:
;;;     action to be taken at a spy port: the default is "stop"

define spy_action =
    newproperty([], 8, "stop", "perm");
enddefine;

;;; spypoint:
;;;     implements a spypoint on a predicate; proc is the unspied value

define lconstant spypoint(proc, pred);
    lvars proc, pred;
    chain(
        if isdefined(pred_alias(pred)) == pred_ident(pred) then
            prolog_maketerm(pred_spec(pred)),
            prolog_break\/1
        else
            proc
        endif);
enddefine;

;;; prolog_call\/1:
;;;     like -call/1-, but unwraps the predicate procedure value to
;;;     removed invisible wrappers like -spypoint-

define prolog_call\/1(/* Goal */);
    lvars Goal = prolog_deref(/* Goal */);
    fast_chain(
        prolog_args_nd(Goal),
        unwrapped(predicate_valof(prolog_termspec(Goal))));
enddefine;

;;; spypoints:
;;;     returns a list of the spypoints set in the current section.
;;;     Used by -debugging/0-.

define spypoints();

    define lconstant le(p1, p2);
        lvars p1, p2, b;
        (alphabefore(Front(p1), Front(p2)) ->> b) == true
        or b == 1 and Back(p1) fi_<= Back(p2);
    enddefine;

    [%  app_predicates(
            procedure(pred);
                lvars pred;
                if pred_spied(pred)
                and isdefined(pred_alias(pred)) == pred_ident(pred)
                then
                    conspair(pred_spec(pred));
                endif;
            endprocedure);
    %], syssort(false, le);
enddefine;

;;; place_spypoint:
;;;     sets a spypoint on a predicate

define lconstant place_spypoint(pred);
    lvars pred;
    true ->> spied -> pred_spied(pred);
    add_wrapper(spypoint, pred_raw_valof(pred), pred, 2) -> pred_raw_valof(pred);
    spymessage(pred_alias(pred), 'Spypoint placed on %p\n');
enddefine;

;;; remove_spypoint:
;;;     removes a spypoint from a predicate

define lconstant remove_spypoint(pred);
    lvars pred;
    (true, false) -> (spied, pred_spied(pred));
    del_wrapper(spypoint, pred_raw_valof(pred)) -> pred_raw_valof(pred);
    spymessage(pred_alias(pred), 'Spypoint removed from %p\n');
enddefine;

;;; spyall:
;;;     places spypoints on everything in the current section which doesn't
;;;     start with "prolog_", which isn't a system predicate and which isn't
;;;     already spied upon.

define lconstant spyall();
    app_predicates(
        procedure(pred);
            lvars pred, fn = pred_functor(pred);
            unless pred_system(pred) == true
            or pred_spied(pred)
            or isword(fn) and isstartstring("prolog_", fn)
            then
                place_spypoint(pred);
            endunless;
        endprocedure);
enddefine;

;;; nospyall:
;;;     removes all spypoints existing in the current section.

define lconstant nospyall();
    app_predicates(
        procedure(pred);
            lvars pred;
            if pred_spied(pred) then
                remove_spypoint(pred);
            endif;
        endprocedure);
enddefine;

;;; tryspy:
;;;     tries placing a spypoint on a predicate

define lconstant tryspy(pred);
    lvars pred;
    if pred_system(pred) == true then
        spymessage(pred_alias(pred), 'Cannot spy on system predicate %p\n');
    elseif pred_spied(pred) then
        spymessage(pred_alias(pred), 'There is already a spypoint on %p\n');
    else
        place_spypoint(pred);
    endif;
enddefine;

;;; trynospy:
;;;     tries removing a spypoint from a predicate

define lconstant trynospy(pred);
    lvars pred;
    if pred_spied(pred) then
        remove_spypoint(pred);
    else
        spymessage(pred_alias(pred), 'There is no spypoint on %p\n');
    endif;
enddefine;

;;; spyon:
;;;     places or removes spypoints on the procedures denoted by -spec-
;;;     according to the value of -action-

define lconstant spyon(spec, action);
    lvars spec, action, fn, arity, pred;
    while ispair(spec) do
        spyon(prolog_arg(1, spec), action);
        prolog_arg(2, spec) -> spec;
    endwhile;
    if isword(spec) then
        app_named_predicates(spec, action);
    elseif prolog_checkspec(spec, "/", 2)
    and isword(prolog_arg(1, spec) ->> fn)
    and isinteger(prolog_arg(2, spec) ->> arity)
    then
        lvars pred = predicate_record(fn, arity, true);
        if pred_isdefined(pred) then
            action(pred);
        else
            spymessage(pred_alias(pred), 'Procedure %p is not defined\n');
        endif;
    elseif spec /== [] then
        mishap(spec, 1, 'ILLEGAL PREDICATE SPECIFICATION');
    endif;
enddefine;

;;; spy:
;;;     setting or removing spypoints from Prolog

define spy(switch);
    lvars   switch;
    dlocal  spied = false;
    if switch == "spy" then
        spyon(tryspy);
    elseif switch == "nospy" then
        spyon(trynospy);
    elseif switch == "spyall" then
        spyall();
    elseif switch == "nospyall" then
        nospyall();
    endif;
    unless spied then
        spymessage(
            if switch == "spy" or switch == "spyall" then
                'No procedures to spy on: nothing done\n'
            else
                'No procedures spied on: nothing done\n'
            endif);
    endunless;
enddefine;

endsection;     /* prolog */


;;; ====  SPY Predicates Defined in Prolog  ================================

PROLOG

:- module prolog.

:- op(254, fx, [spy, nospy]).

:- dynamic spy_action_hook/3.

% Setting and removing spypoints

'spy' :-
    prolog_eval('spy'(spyall)).

'spy'(Spec) :-
    prolog_eval('spy'(quote(Spec), 'spy')).

'nospy' :-
    prolog_eval('spy'(nospyall)).

'nospy'(Spec) :-
    prolog_eval('spy'(quote(Spec), 'nospy')).

nodebug :-
    'nospy'.

debugging :-
    prolog_eval(apply(valof(spypoints)), L),
    L \= [],
    !,
    format("There are spypoints on the following procedures:~n"),
    prolog_display_spypoints(L),
    prolog_display_actions.
debugging :-
    format("There are no spypoints~n").

prolog_display_spypoints(L) :-
    member([F|A], L),
    format("~c~w/~w~n", [9, F, A]),
    fail.
prolog_display_spypoints(_).


% Controlling the spy debugger

quietspy(X) :-
    var(X),
    !,
    (prolog_evaltrue(valof(prolog_quiet_spy)) ->
        X = on
    ;   X = off
    ).
quietspy(on) :-
    prolog_setq(prolog_quiet_spy, valof(true)),
    !.
quietspy(off) :-
    prolog_setq(prolog_quiet_spy, valof(false)).

leash :-
    prolog_change_actions(continue, stop).

unleash :-
    prolog_change_actions(stop, continue).

leash(Port) :-
    spy_action(Port, stop).

unleash(Port) :-
    spy_action(Port, continue).

prolog_change_actions(Action1, Action2) :-
    current_spy_action(Port, Action1),
    spy_action(Port, Action2),
    fail.
prolog_change_actions(_, _) :-
    prolog_display_actions.

spy_action(Action) :-
    spy_action(all, Action).

spy_action(Port, Action) :-
    var(Port),
    !,
    prolog_port_check(Port).
spy_action(Port, Action) :-
    prolog_port(Port),
    !,
    prolog_action_check(Action),
    prolog_set_action(Port, Action).
spy_action(all, Action) :-
    !,
    (   prolog_port(Port),
        spy_action(Port, Action),
        fail
    ;   true
    ).
spy_action([Port|Ports], Action) :-
    !,
    spy_action(Port, Action),
    spy_action(Ports, Action).
spy_action([], Action) :-
    !,
    prolog_action_check(Action).
spy_action(Port, _) :-
    prolog_port_check(Port).

current_spy_action(Action) :-
    current_spy_action(all, Action).

current_spy_action(Port, Action) :-
    var(Port),
    !,
    prolog_port(Port),
    prolog_action(Port, Action).
current_spy_action(Port, Action) :-
    prolog_port(Port),
    !,
    prolog_action(Port, Action).
current_spy_action(all, Action) :-
    !,
    % Are all ports set to the same action?
    setof(Action1, Port^current_spy_action(Port,Action1), [Action]).
current_spy_action([Port|Ports], Action) :-
    !,
    current_spy_action(Port, Action),
    current_spy_action(Ports, Action).
current_spy_action([], Action) :-
    prolog_action(Action).

prolog_port_check(Port) :-
    nonvar(Port),
    prolog_port(Port),
    !.
prolog_port_check(Port) :-
    error('ILLEGAL SPY PORT NAME', [Port]).

prolog_port(Port) :-
    prolog_port(Port, _).

prolog_port(call, 'Call').
prolog_port(exit, 'Exit').
prolog_port(redo, 'Redo').
prolog_port(fail, 'Fail').

prolog_action_check(Action) :-
    nonvar(Action),
    prolog_action(Action),
    !.
prolog_action_check(Action) :-
    error('ILLEGAL SPY ACTION NAME', [Action]).

prolog_action(stop).
prolog_action(continue).
prolog_action(ignore).
prolog_action(user).

prolog_action(Port, Action) :-
    prolog_eval(spy_action(quote(Port)), Action).

prolog_set_action(Port, Action) :-
    prolog_eval(apply(quote(Action,Port), updater(valof(spy_action)))).

prolog_display_actions :-
    current_spy_action(Action),
    % Reproduce old messages if all ports are leashed or unleashed
    (   Action = stop ->
            format("Spyports are leashed~n")
    ;   Action = continue ->
            format("Spyports are unleashed~n")
    ;   Action = ignore ->
            format("Spyports are being ignored~n")
    ;   fail
    ),
    !.
prolog_display_actions :-
    format("Spyport actions:~n"),
    prolog_port(Port),
    prolog_action(Port, Action),
    format("~c~w: ~w~n", [9, Port, Action]),
    fail.
prolog_display_actions.


% prolog_break(+Goal)
%   calls Goal under the control of the "spy" debugger

prolog_break(Goal) :-
    prolog_val(oride, no),
    prolog_val(push, any),
    prolog_val(skip, 0),
    !,
    prolog_eval(valof(invoc)+1, I),
    prolog_setq(invoc, quote(I)),
    prolog_spypoint(Goal, I).
prolog_break(Goal) :-
    prolog_spycall(Goal).

prolog_spypoint(Goal, I) :-
    prolog_spypoint(call, Goal, I),
    (   prolog_spycall(Goal)
    ;   prolog_spypoint(fail, Goal, I),
        prolog_must_fail(I),
        !,
        fail
    ),
    (   prolog_spypoint(exit, Goal, I),
        prolog_val(push, any)
    ;   prolog_spypoint(redo, Goal, I),
        fail
    ).
prolog_spypoint(Goal, I) :-
    % This for retries
    prolog_spypoint(Goal, I).

prolog_spycall(Goal) :-
    prolog_val(push, any),
    prolog_call(Goal).

prolog_must_fail(I) :-
    prolog_val(push, any),
    !.
prolog_must_fail(I) :-
    prolog_val(invoc, I1),
    I1 =\= I.

prolog_spypoint(Port, Goal, I) :-
    prolog_stop_here(Port, Goal, I, Intro),
    !,
    prolog_action(Port, Action),
    prolog_spypoint(Action, Port, Goal, I, Intro).
prolog_spypoint(_, _, _).

prolog_stop_here(Port, Goal, I, Intro) :-
    prolog_stop_pushing(Port, I),
    prolog_stop_skipping(I, Intro),
    functor(Goal, F, N),
    prolog_spied(F, N).

prolog_stop_pushing(_, _) :-
    prolog_val(push, any),
    !.
prolog_stop_pushing(_, _) :-
    prolog_val(push, redo),
    !,
    prolog_setq(push, any).
prolog_stop_pushing(Port, I) :-
    prolog_val(push, Port),
    prolog_val(invoc, I),
    prolog_setq(push, any).

prolog_stop_skipping(_, '**') :-
    prolog_val(skip, 0),
    !.
prolog_stop_skipping(I, '*>') :-
    prolog_val(skip, I),
    prolog_setq(skip, 0).

prolog_spied(F, N) :-
    prolog_evaltrue(pred_spied(predicate_record(quote(F, N), valof(true)))).

prolog_spypoint(stop, Port, Goal, I, Intro) :-
    !,
    seeing(In), see(user),
    telling(Out), tell(user),
    prolog_display_goal(Port, Goal, I, Intro),
    repeat,
        prolog_get_command(Port, Command),
        prolog_do_command(Command, Port, Goal, I, Intro),
        !,
    see(In), tell(Out).
prolog_spypoint(continue, Port, Goal, I, Intro) :-
    !,
    telling(Out), tell(user),
    prolog_display_goal(Port, Goal, I, Intro), nl,
    tell(Out).
prolog_spypoint(ignore, _, _, _, _) :-
    !.
prolog_spypoint(user, Port, Goal, I, _) :-
    prolog_spy_action_hook(Port, Goal, I).

prolog_spy_action_hook(Port, Goal, I) :-
    prolog_evaltrue(predicate_has_clauses(spy_action_hook, 3)),
    prolog_setq(oride, yes),    % Disable spy while running user actions
    spy_action_hook(Port, Goal, I),
    !,
    prolog_setq(oride, no).
prolog_spy_action_hook(_, _, _).


% Interactive tracing

prolog_display_goal(Port, Goal, I, Intro) :-
    prolog_display_goal(print, Port, Goal, I, Intro).

prolog_display_goal(Pred, Port, Goal, I, Intro) :-
    prolog_port(Port, Label),
    !, % Remove choicepoint for prolog_port/2
    format("~w (~d) ~w : ", [Intro, I, Label]),
    prolog_display_goal(Pred, Goal).

prolog_display_goal(print, Goal) :-
    !,
    prolog_setq(oride, yes),    % Disable spy while printing
    print(Goal),
    prolog_setq(oride, no).
prolog_display_goal(write, Goal) :-
    !,
    write(Goal).
prolog_display_goal(display, Goal) :-
    display(Goal).


% Debugger commands

prolog_get_command(Port, Command) :-
    prolog_eval(prolog_readline('? '), Resp),
    prolog_get_command(Resp, Port, Command).

prolog_get_command([Resp|_], Port, Command) :-
    !,
    prolog_get_command(Resp, Port, Command).
prolog_get_command(Resp, Port, Command) :-
    prolog_valid_command(Resp, Port, Command),
    !.
prolog_get_command(Resp, Port, Resp) :-
    prolog_valid_command(_, Port, Resp),
    !.
prolog_get_command(Resp, Port, menu) :-
    prolog_command(Resp, Command),
    !,
    format("Cannot ~w at the ~w port~n", [Command, Port]).
prolog_get_command(Resp, Port, menu) :-
    format("No such command: ~w~n", [Resp]).

prolog_valid_command(Abbrev, Port, Command) :-
    prolog_command(Abbrev, Command),
    prolog_valid_command(Port, Command).

prolog_valid_command(Port, Command) :-
    prolog_invalid_command(Port, Command),
    !,
    fail.
prolog_valid_command(_, _).

prolog_invalid_command(call, backtrack).
prolog_invalid_command(call, retry).
prolog_invalid_command(exit, skip).
prolog_invalid_command(redo, backtrack).
prolog_invalid_command(fail, backtrack).
prolog_invalid_command(fail, fail).
prolog_invalid_command(fail, skip).

prolog_command([], continue).
prolog_command(c, continue).
prolog_command(';', backtrack).
prolog_command(f, fail).
prolog_command(r, retry).
prolog_command(s, skip).
prolog_command(p, print).
prolog_command(w, write).
prolog_command(d, display).
prolog_command(l, listing).
prolog_command(u, unleash).
prolog_command(o, nodebug).
prolog_command(b, break).
prolog_command(a, abort).
prolog_command(e, exit).
prolog_command(h, help).
prolog_command(?, menu).

% Command execution:
prolog_do_command(continue, _, _, _, _) :-
    !.
prolog_do_command(backtrack, _, _, _, _) :-
    !,
    prolog_setq(push, redo).
prolog_do_command(fail, _, _, I, _) :-
    !,
    prolog_setq(push, fail),
    prolog_setq(invoc, quote(I)).
prolog_do_command(retry, _, _, I, _) :-
    !,
    prolog_setq(push, call),
    prolog_setq(invoc, quote(I)),
    format("[retry]~n").
prolog_do_command(skip, _, _, I, _) :-
    !,
    prolog_setq(skip, quote(I)).
% Display actions
prolog_do_command(print, Port, Goal, I, Intro) :-
    !,
    prolog_display_goal(print, Port, Goal, I, Intro),
    fail.
prolog_do_command(write, Port, Goal, I, Intro) :-
    !,
    prolog_display_goal(write, Port, Goal, I, Intro),
    fail.
prolog_do_command(display, Port, Goal, I, Intro) :-
    !,
    prolog_display_goal(display, Port, Goal, I, Intro),
    fail.
prolog_do_command(listing, Port, Goal, I, Intro) :-
    !,
    functor(Goal, F, N),
    listing(F/N),
    prolog_display_goal(Port, Goal, I, Intro),
    fail.
% Debugger control actions
prolog_do_command(unleash, _, _, _, _) :-
    !,
    unleash.
prolog_do_command(nodebug, _, _, _, _) :-
    !,
    nodebug.
prolog_do_command(break, Port, Goal, I, Intro) :-
    !,
    break,
    prolog_display_goal(Port, Goal, I, Intro),
    fail.
prolog_do_command(abort, _, _, _, _) :-
    !,
    abort.
prolog_do_command(exit, Port, Goal, I, Intro) :-
    !,
    prolog_eval(prolog_readline('Do you really want to leave Prolog? '), Resp),
    (Resp = [yes] ->
        halt
    ;   prolog_display_goal(Port, Goal, I, Intro),
        fail
    ).
prolog_do_command(help, Port, Goal, I, Intro) :-
    !,
    prolog_eval(pop11_compile(quote([help,spy]))),
    prolog_display_goal(Port, Goal, I, Intro),
    fail.
prolog_do_command(menu, Port, Goal, I, Intro) :-
    !,
    prolog_display_menu(Port),
    prolog_display_goal(Port, Goal, I, Intro),
    fail.


% Display a menu of available commands

prolog_display_menu(Port) :-
    format("Commands available at the ~w port are:~n", [Port]),
    prolog_command([], Default),
    prolog_valid_command(Option, Port, Command),
    prolog_display_option(Option, Command, Default),
    fail.
prolog_display_menu(_).

prolog_display_option([], _, _) :-
    !.
prolog_display_option(Option, Command, Default) :-
    format("~c~w~c~w", [9, Option, 9, Command]),
    (   Command = Default ->
        format(" (the default)~n")
    ;   nl
    ).

:- endmodule prolog.

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 15 1993
        Renamed from old "debugpreds.p". Changed the spying mechanism to
        work directly on Prolog procedures rather than Pop-11 continuation
        ones, which saves two redundant closures for each spy point.
--- Robert John Duncan, Apr  9 1992
        Introduced format/[1,2]
--- Robert Duncan, Jan 27 1992
        Added -spy_action- and related predicates
--- Simon Nichols, Nov 15 1991
        Changed prolog_act/5 to refer the user to HELP SPY rather than
        HELP OPTIONS.
--- Robert John Duncan, Jun 24 1991
        Renamed define forms.
--- Andrew Casson, Sep  3 1990
        Added missing cuts to disjunctions in 'spy'/1 and 'nospy'/1.
--- Rob Duncan, Feb 16 1990
        Fixed calls to -p_spied- to expect 0/1 result instead of boolean.
        Replaced calls to -identof- with -isdefined- to check for the
        existence of the identifier first.
--- Rob Duncan, Aug  8 1989
        Sectionised and added #_INCLUDEs for POPC.
        Changed definition of 'prolog_callproc/1' to use -define:predicate-.
 */
