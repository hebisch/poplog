/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/basicpreds.p
 > Purpose:         Prolog: support for basic built-in predicates
 > Author:          Rob Duncan & Simon Nichols, Nov  3 1987 (see revisions)
 > Related files:
 */


section prolog;

constant
    procedure ( bad_goal, prolog_atomic, ),
;

;;; ========================================================================

;;; inline_functor:
;;;     used for inline calls to functor/3

define inline_functor(/*Term,*/ Fn, Arity) with_props functor\/3 with_nargs 3;
    lvars Term = prolog_deref(/*Term*/), Fn, Arity, fn, arity;
    if isprologvar(Term) then
        ;;; Construct a new term with given functor name and arity
        if isprologvar(prolog_deref(Fn) ->> Fn)
        or isprologvar(prolog_deref(Arity) ->> Arity)
        then
            bad_goal(Term, Fn, Arity, "functor", 3);
        elseif not(isinteger(Arity)) then
            mishap(Arity, 1, 'INTEGER NEEDED');
        elseif Arity fi_< 0 then
            return(false);
        endif;
        prolog_assign(Term,
            prolog_maketerm(
                fast_repeat Arity times prolog_newvar() endrepeat,
                Fn, Arity));
        true;
    else
        ;;; Decompose Term into functor and arity
        lvars (fn, arity) = prolog_termspec(Term);
        prolog_unify(Fn, fn) and prolog_unify(Arity, arity);
    endif;
enddefine;

;;; inline_arg:
;;;     used for inline calls to arg/3

define inline_arg(/*I, Term,*/ Arg) with_props arg\/3 with_nargs 3;
    lvars I, Term, Arg;
    if isprologvar(prolog_deref(/*Term*/) ->> Term)
    or isprologvar(prolog_deref(/*I*/) ->> I)
    then
        bad_goal(I, Term, Arg, "arg", 3);
    elseif not(isinteger(I)) then
        mishap(I, 1, 'INTEGER NEEDED');
    elseif isprologterm(Term) then
        I fi_> 0 and I fi_<= fast_prolog_arity(Term)
        and prolog_unify(fast_prolog_arg(I, Term), Arg);
    elseif ispair(Term) then
        I == 1 and prolog_unify(fast_front(Term), Arg)
        or I == 2 and prolog_unify(fast_back(Term), Arg);
    else
        false;
    endif;
enddefine;

;;; inline_univ:
;;;     used for inline calls to =../2

define inline_univ(/*Term,*/ List) with_props \=\.\.\/2 with_nargs 2;
    lvars l, Term, List, fn, arity;
    if isprologvar(prolog_deref(/*Term*/) ->> Term) then
        if isprologvar(prolog_deref(List) ->> List) then
            bad_goal(Term, List, "'=..'", 2);
        elseif ispair(List) then
            prolog_deref(Destpair(List) -> l) -> fn;
            if isprologvar(fn) then
                bad_goal(Term, List, "'=..'", 2);
            endif;
            0 -> arity;
            while ispair(prolog_deref(l) ->> l) do
                arity fi_+ 1 -> arity;
                Destpair(l) -> l;
            endwhile;
            if l == [] then
                prolog_maketerm(fn, arity) -> l;
                prolog_assign(Term, l);
                true;
            else
                erasenum(arity);
                if isprologvar(l) then
                    bad_goal(Term, List, "'=..'", 2);
                endif;
                false;
            endif;
        else
            false;
        endif;
    else
        prolog_unify(
            List,
            [% prolog_functor(Term), prolog_args_nd(Term) %]);
    endif;
enddefine;

;;; inline_name:
;;;     used for inline calls to name/2

define inline_name(/*X,*/ String) with_props name\/2 with_nargs 2;
    lvars s, c, cnt, all_numbercodes, X, String;
    if isprologvar(prolog_deref(/*X*/) ->> X) then
        prolog_deref(String) -> s; 0 -> cnt; true -> all_numbercodes;
        while ispair(s) do
            prolog_deref(prolog_deref(Destpair(s)) -> s) -> c;
            c; cnt fi_+ 1 -> cnt;
            unless isnumbercode(c) then
                false -> all_numbercodes;
            endunless;
        endwhile;
        if isprologvar(s) then
            bad_goal(X, String, "name", 2);
        elseif s /== [] then
            mishap(String, 1, 'LIST NEEDED');
        endif;
        if cnt fi_> 0 and all_numbercodes then
            strnumber(consstring(cnt))
        else
            consword(cnt)
        endif -> s;
        prolog_assign(X, s);
        true;
    elseif prolog_atomic(X) then
        prolog_unify(String, [% dest_characters(X) %]);
    else
        mishap(X, 1, 'ATOM NEEDED');
    endif;
enddefine;

;;; inline_length:
;;;     used for inline calls to length/2

define inline_length(L, N) with_props length\/2;
    lvars n = 0, L, N;
    while ispair(prolog_deref(L) ->> L) do
        n fi_+ 1 -> n;
        Back(L) -> L;
    endwhile;
    L == [] and prolog_unify(N, n);
enddefine;

;;; numbervars:
;;;     used for inline calls to numbervars/3

define numbervars(term, n) -> n;
    lvars i, n, term;
    if isprologvar(term) then
        prolog_assign(term, prolog_maketerm(n, "'$VAR'", 1));
        n + 1 -> n;
    elseif prolog_complexterm(term) then
        For i to prolog_arity(term) do
            numbervars(prolog_arg(i, term), n) -> n;
        endfor;
    endif;
enddefine;

endsection;     /* prolog */

PROLOG

:- op(40, xfx, [is, <, >, >=, =<, =:=, =\=, =.., =, \=, ==, \==]).
:- op(31, yfx, [+, -, \/, /\]).
:- op(21, yfx, [*, /, div, mod, >>, <<]).
:- op(21, fx,  [+, -, \]).

:- module prolog.

:- inline((
        atom(X) :-
            prolog_evaltrue(prolog_atom(quote(X)))
    )).

:- inline((
        atomic(X) :-
            prolog_evaltrue(prolog_atomic(quote(X)))
    )).

:- inline((
        var(X) :-
            prolog_evaltrue(isprologvar(quote(X)))
    )).

:- inline((
        nonvar(X) :-
            prolog_evaltrue(not(isprologvar(quote(X))))
    )).

:- inline((
        integer(X) :-
            prolog_evaltrue(isintegral(quote(X)))
    )).

:- inline((
        float(X) :-
            prolog_evaltrue(isdecimal(quote(X)))
    )).

:- inline((
        X = Y :-
            prolog_evaltrue(prolog_unify(quote_nd(X,Y)))
    )).

:- inline((
        X == Y :-
            prolog_evaltrue('='(quote_nd(X,Y)))
    )).

:- inline((
        X \== Y :-
            prolog_evaltrue(not('='(quote_nd(X,Y))))
    )).

:- inline((
        X =:= Y :-
            prolog_evaltrue('='(X,Y))
    )).

:- inline((
        X =\= Y :-
            prolog_evaltrue(not('='(X,Y)))
    )).

:- inline((
        X < Y :-
            prolog_evaltrue('<'(X,Y))
    )).

:- inline((
        X =< Y :-
            prolog_evaltrue('<='(X,Y))
    )).

:- inline((
        X > Y :-
            prolog_evaltrue('>'(X,Y))
    )).

:- inline((
        X >= Y :-
            prolog_evaltrue('>='(X,Y))
    )).

:- inline((
        X is Expr :-
            prolog_eval(Expr, X)
    )).

:- inline((
        functor(X, Fn, Arity) :-
            prolog_evaltrue(inline_functor(quote_nd(X, Fn, Arity)))
    )).

:- inline((
        arg(I, X, Arg) :-
            prolog_evaltrue(inline_arg(quote_nd(I, X, Arg)))
    )).

:- inline((
        X =.. L :-
            prolog_evaltrue(inline_univ(quote_nd(X, L)))
    )).

:- inline((
        name(X, S) :-
            prolog_evaltrue(inline_name(quote_nd(X, S)))
    )).

:- inline((
        length(L, N) :-
            prolog_evaltrue(inline_length(quote_nd(L, N)))
    )).

:- inline((
        numbervars(X, N, M) :-
            prolog_eval(numbervars(quote(X, N)), M)
    )).


% Special definition for \= /2: can't be inlined because we have to
% undo any variable bindings on failure

X \= X :-
    !, fail.
X \= Y.

% Control predicates not defined in "control.p"

not(Goal) :-
    Goal, !, fail.
not(Goal).

incore(Goal) :-
    call(Goal).

G -> H :-
    G, !, H.

% Locals (not exported from section prolog)

member(X, [X|_]).
member(X, [_|Xs]) :-
    member(X, Xs).

append([], L, L).
append([X|Xs], Ys, [X|Zs]) :-
    append(Xs, Ys, Zs).

:- endmodule prolog.


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug 19 1993
        Fixed inline definition of nonvar/1.
--- Robert John Duncan, Jul 15 1993
        Changed to new inline strategy. Moved in some Prolog definitions
        from "controlpreds.p".
--- Robert John Duncan, Jun 24 1991
        Renamed define forms.
--- Simon Nichols, Oct 22 1990
        Changed functor/3 and arg/3 to fail on out-of-range integer
        arguments instead of raising an error.
--- Rob Duncan, Aug  8 1989
    Sectionised and added #_INCLUDEs for POPC; moved out -prolog_atom- and
    -prolog_atomic- to "util.p"; modified POP-11 definitions to use the
    revised define forms: predicate, inline, optimisable.
--- Rob Duncan, Jun 29 1989
    Changed -prolog_atomic- to look at the item key rather than its prolog
    type so that the macros for the type could be removed from "util.p"
--- Rob Duncan, Sep  5 1988
    Rewrote to make use of -prolog_pdr- and -prolog_inline_pdr-
 */
