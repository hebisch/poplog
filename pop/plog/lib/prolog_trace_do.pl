/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/lib/prolog_trace_do.pl
 > Purpose:         Common interface stuff from LIBRARY *CT, *TRACER
 > Author:          Robert John Duncan, Aug 20 1993
 > Documentation:
 > Related Files:
 */

:- prolog_language(pop11).

section;

global vars
    prolog_trace_do = "prolog_trace_do",
        ;;; determines whether ?-/1 should trace
    prolog_trace_mode = "none",
        ;;; which tracer to use
;

;;; ??:
;;;     syntax word like ?- with tracing

define global vars syntax ??;
    dlocal prolog_trace_do = "prolog_trace_do";
    nonsyntax ?-();
enddefine;

endsection;

:- prolog_language(prolog).

:- current_op(Prec, Fix, (?-)),
        op(Prec, Fix, (??)).

:- global prolog_trace_do/1.
:- global prolog_trace_mode/2.
:- dynamic prolog_trace_mode/2.

prolog_trace_do(Goal) :-
    prolog_val(prolog_trace_mode, Mode),
    prolog_trace_mode(Mode, Tracer), !,
    call(Tracer, Goal).
prolog_trace_do(Goal) :-
    call(Goal).

% Make ?- work like ??
trace_do :-
    prolog_setq(prolog_trace_do, prolog_trace_do),
    format("on~n").

% unset the above
no_trace_do :-
    prolog_setq(prolog_trace_do, valof(false)),
    format("off~n").


% Use term_expansion/2 to automate tracing of queries

:- retractall(
        term_expansion((?? Goal), New)
    ).
:- asserta((
        term_expansion((?? Goal), New) :- !,
            New = (?-(prolog_trace_do(Goal)))
    )).

:- retractall(
        term_expansion((?- Goal), New)
    ).
:- asserta((
        term_expansion((?- Goal), New) :-
            prolog_evaltrue(valof(prolog_trace_do)), !,
            New = (?-(prolog_trace_do(Goal)))
    )).
