/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/findall.p
 > Purpose:         Prolog: all-solutions predicates (findall, bagof, setof)
 > Author:          Robert John Duncan, Jun 22 1993
 > Documentation:
 > Related Files:
 */

section prolog;

;;; vars_in:
;;;     adds to the list -vs- all the variables which occur in -term-

define lconstant vars_in(term, vs) -> vs;
    lvars term, vs;
    repeat
        go_on prolog_type(prolog_deref(term) ->> term) to
            var pair term novars novars novars;
        var:
            unless fast_lmember(term, vs) then
                conspair(term, vs) -> vs;
            endunless;
            return;
        pair:
            vars_in(fast_front(term), vs) -> vs;
            fast_back(term) -> term;
            nextloop;
        term:
            lvars i;
            For i to fast_prolog_arity(term) fi_- 1 do
                vars_in(fast_prolog_arg(i, term), vs) -> vs;
            endfor;
            fast_prolog_arg(i, term) -> term;
            nextloop;
        novars:
            return;
    endrepeat;
enddefine;

;;; free_vars_in:
;;;     adds to -fvs- all the free variables which occur in -term-.
;;;     A free variable is one which is not bound: initially, all bound
;;;     variables are listed in -bvs-, but some special terms bind extra
;;;     variables in their arguments.

define lconstant free_vars_in(term, bvs, fvs) -> fvs;
    lvars term, bvs, fvs;
    repeat
        go_on prolog_type(prolog_deref(term) ->> term) to
            var pair term novars novars novars;
        var:
            unless fast_lmember(term, bvs) or fast_lmember(term, fvs) then
                conspair(term, fvs) -> fvs;
            endunless;
            return;
        pair:
            free_vars_in(fast_front(term), bvs, fvs) -> fvs;
            fast_back(term) -> term;
            nextloop;
        term:
            ;;; look out for nested bindings
            lvars (fn, arity) = prolog_termspec(term);
            if fn == "^" and arity == 2 then
                ;;; X^T : X is bound in T
                vars_in(fast_prolog_arg(1, term), bvs) -> bvs;
                fast_prolog_arg(2, term) -> term;
                nextloop;
            elseif (fn == "setof" or fn == "bagof") and arity == 3 then
                ;;; setof(X, T, L) : X is bound in T
                free_vars_in(fast_prolog_arg(3, term), bvs, fvs) -> fvs;
                vars_in(fast_prolog_arg(1, term), bvs) -> bvs;
                fast_prolog_arg(2, term) -> term;
                nextloop;
            elseif fn == "findall" and arity == 3 then
                ;;; findall(X, T, L) : X+T has no free vars
                fast_prolog_arg(3, term) -> term;
                nextloop;
            elseif fn == "\+" and arity == 1 then
                ;;; \+ T : T has no free vars
                return;
            else
                lvars i;
                For i to arity fi_- 1 do
                    free_vars_in(fast_prolog_arg(i,term), bvs, fvs) -> fvs;
                endfor;
                fast_prolog_arg(arity, term) -> term;
                nextloop;
            endif;
        novars:
            return;
    endrepeat;
enddefine;

;;; all_solutions:
;;;     returns a list of all instances of X for which Goal succeeds
;;;     (in reverse order).

define lconstant all_solutions(X, Goal) -> solns;
    lvars X, Goal, solns = [];

    define lconstant save_soln();
        conspair(prolog_generalise(X), solns) -> solns;
    enddefine;

    SAVE;
    prolog_push_continuation(save_soln, "apply", 2);
    call\/1(Goal);
    RESTORE;
enddefine;

;;; findall/3:

define findall\/3(/* X, Goal, */ Bag) with_nargs 3;
    lvars Bag, solns = all_solutions(/* X, Goal, */);
    if prolog_unify(Bag, fast_ncrev(prolog_instance(solns))) then
        chain(prolog_apply_continuation);
    endif;
enddefine;

;;; bagof/3:

define bagof\/3(X, Goal, Bag);
    lvars soln, solns, solns1, prev, curr, free, boundvars, freevars,
          X, Goal, Bag;
    SAVE;
    vars_in(X, []) -> boundvars;
    ;;; strip off leading existential qualifiers to improve the call
    prolog_deref(Goal) -> Goal;
    while prolog_checkspec(Goal, "^", 2) do
        vars_in(fast_prolog_arg(1, Goal), boundvars) -> boundvars;
        prolog_arg(2, Goal) -> Goal;
    endwhile;
    free_vars_in(Goal, boundvars, []) -> freevars;
    if freevars == [] then
        ;;; no free variables - bagof can only succeed once
        all_solutions(X, Goal) -> solns;
        if solns /== []
        and prolog_unify(Bag, fast_ncrev(prolog_instance(solns)))
        then
            chain(prolog_apply_continuation);
        endif;
    else
        ;;; must provide alternative solutions for each particular
        ;;; instantiation of the free variables
        consprologterm(dl(freevars), "free", length(freevars)) -> freevars;
        all_solutions(consprologterm(freevars, X, "-", 2), Goal) -> solns;
        until solns == [] do
            ;;; pick out the first solution ...
            Front(solns) -> soln;
            conspair(fast_prolog_arg(2, soln), []) -> solns1;
            ;;; ... then extract from -solns- all others which have the same
            ;;; instantiation of free variables
            fast_prolog_arg(1, soln) -> free;
            solns -> prev;
            until (Back(prev) ->> curr) == [] do
                Front(curr) -> soln;
                if fast_prolog_arg(1, soln) = free then
                    ;;; include it ...
                    conspair(fast_prolog_arg(2, soln), solns1) -> solns1;
                    ;;; ... and delete it from -solns-
                    Back(curr) -> Back(prev);
                else
                    curr -> prev;
                endif;
            enduntil;
            if prolog_unify(freevars, prolog_instance(free))
            and prolog_unify(Bag, prolog_instance(solns1))
            then
                prolog_apply_continuation();
            endif;
            RESTORE;
            Back(solns) -> solns;
        enduntil;
    endif;
enddefine;

endsection;     /* prolog */

PROLOG

:- op(10, xfy, ^).

:- module prolog.

_ ^ G :-
    G.

fast_bagof(X, P, L) :-
    findall(X, P, L),
    L \== [].

fast_setof(X, P, S) :-
    findall(X, P, L),
    L \== [],
    sort(L, S).

setof(X, P, S):-
    bagof(X, P, L),
    sort(L, S).

:- endmodule prolog.
