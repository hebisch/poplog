/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/plog/lib/prolog_syntax.pl
 > Purpose:         Allow alternative syntaxes to be provided for Prolog
 > Author:          Simon Nichols, Feb  2 1990 (see revisions)
 > Documentation:
 > Related Files:   C.all/plog/src/parse.p
 */

% Please note: this library file is experimental and unsupported (at present).

:- module prolog.
:- export prolog_syntax/1.

:- system_predicate
    prolog_syntax/1,
    val_in_module/2,
    val_in_module_goal/3.

:- no_clauses
    prolog_syntax/1,
    val_in_module/2,
    val_in_module_goal/3.

:- dynamic
    prolog_syntax_table/4,
    current_syntax/1.


% prolog_syntax_table(?SyntaxName, ?ReaderName, ?ExpandFlag, ?Terminator)
%   table containing an entry (i.e. a fact) for each available syntax.

prolog_syntax_table(poplog, $-prolog$-'Readterm', true, '.').


% current_syntax(?SyntaxName)
%   the currently selected syntax.

current_syntax(poplog).


% prolog_syntax(?SyntaxName)
%   change syntax or enquire about the current syntax.

prolog_syntax(SyntaxName) :-
    var(SyntaxName), !,
    current_syntax(SyntaxName).
prolog_syntax(SyntaxName) :-
    prolog_syntax_table(SyntaxName, ReaderName, ExpandFlag, Terminator), !,
    val_in_module(ReaderName, ReaderPdr),
    prolog_setq(readterm, ReaderPdr),
    prolog_setq(prolog_expand_macros, valof(ExpandFlag)),
    prolog_setq(prolog_term_terminator, Terminator),
    $-prolog$-retract(current_syntax(_)),
    $-prolog$-assert(current_syntax(SyntaxName)).


% val_in_module(+PopVar, ?Value)
%   similar to prolog_val/2, except it copes with module pathnames.

val_in_module(PopVar, Value) :-
    val_in_module_goal(PopVar, Value, Goal),
    Goal.

% val_in_module_goal(+Path, +Value, ?Goal)
%   called by val_in_module/2 to construct a prolog_val/2 goal with Path as a
%   prefix.

val_in_module_goal($-Path, Value, $-Path1) :- !,
    val_in_module_goal(Path, Value, Path1).
val_in_module_goal(Module$-Path, Value, Module$-Path1) :- !,
    val_in_module_goal(Path, Value, Path1).
val_in_module_goal(PopVar, Value, prolog_val(PopVar, Value)).

:- endmodule prolog.

/* --- Revision History ---------------------------------------------------
--- Simon Nichols, Oct 24 1991
        Changed name of library from "syntax.pl" to "prolog_syntax.pl" and
        the name of the exported predicate from syntax/1 to prolog_syntax/1.
 */
