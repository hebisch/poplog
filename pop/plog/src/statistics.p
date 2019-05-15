/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/statistics.p
 > Purpose:         Prolog: report statistics about the Prolog system
 > Author:          Robert John Duncan, Jul  5 1993
 > Documentation:
 > Related Files:
 */

PROLOG

:- module prolog.

statistics :-
    prolog_eval(valof(popgctime) / 100, GCTime),
    prolog_eval(apply(valof(systime)) / 100, Time),
    format("CPU time used: ~2f seconds. GC time: ~2f seconds.~n",
        [Time, GCTime]),
    prolog_eval(valof(popmemused), Used),
    prolog_eval(valof(popmemlim) - Used, Free),
    format("At last garbage collection: ~d words used and ~d words free.~n",
        [Used, Free]).

%   statistics/1:
%       useful clauses

statistics(heap, [Used,Free]) :-
    prolog_eval(valof(popmemused), Used),
    prolog_eval(valof(popmemlim) - Used, Free).
statistics(runtime, [Abs, Delta]) :-
    prolog_eval(apply(valof(systime)) * 10, Abs),
    prolog_eval(intof(apply(valof(timediff)) * 1000), Delta).
statistics(garbage_collection, [Count, Freed, Time]) :-
    prolog_eval(valof(popgctime) * 10, Time).

%   not-so-useful clauses, kept for compatibility with DEC-10

statistics(core, [Low,High]).
statistics(global_stack, [Used, Free]).
statistics(local_stack, [Used, Free]).
statistics(trail, [Used, Free]).
statistics(stack_shifts, [Locals, Trail, Time]).

:- endmodule prolog.
