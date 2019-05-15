/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/controlpreds.p
 > Purpose:         Prolog: primitive control predicates
 > Author:          Rob Duncan & Simon Nichols, Nov  3 1987 (see revisions)
 > Documentation:
 > Related Files:
 */


section prolog;

constant
    procedure ( predicate_valof, ),
;

;;; ========================================================================

;;; fail/0:
;;;     never succeeds

define fail\/0;
compile_mode:vm -pentch;
enddefine;

;;; true/0:
;;;     succeeds exactly once

define true\/0;
compile_mode:vm -pentch;
    chain(prolog_apply_continuation);
enddefine;

;;; repeat/0:
;;;     succeeds any number of times

define repeat\/0;
    SAVE;
    repeat
        prolog_apply_continuation();
        RESTORE;
    endrepeat;
enddefine;

;;; call/[1,2,3]:
;;;     specialised versions of call/N for N = 1,2,3

define call\/1(/* G */) with_nargs 1;
    lvars G = prolog_deref(/* G */);
    fast_chain(prolog_args_nd(G), predicate_valof(prolog_termspec(G)));
enddefine;

define call\/2(G, X);
    lvars G, X;
    prolog_deref(G) -> G;
    if isword(G) then
        fast_chain(X, predicate_valof(G, 1));
    else
        lvars (fn, arity) = prolog_termspec(G);
        fast_chain(prolog_args_nd(G), X, predicate_valof(fn, arity fi_+ 1));
    endif;
enddefine;

define call\/3(G, X, Y);
    lvars G, X, Y;
    prolog_deref(G) -> G;
    if isword(G) then
        fast_chain(X, Y, predicate_valof(G, 2));
    else
        lvars (fn, arity) = prolog_termspec(G);
        fast_chain(prolog_args_nd(G), X, Y, predicate_valof(fn, arity fi_+ 2));
    endif;
enddefine;

;;; \+/1:
;;;     fail-if

define \\\+\/1(/* G */) with_nargs 1;
    lvars G = prolog_deref(/* G */);
    SAVE;
    prolog_push_continuation("prolog_own_exit_on_success", 1);
    unless prolog_own_invoke(prolog_args_nd(G),
            predicate_valof(prolog_termspec(G)))
    then
        RESTORE;
        chain(prolog_apply_continuation);
    endunless;
enddefine;

;;; ,/2:
;;;     conjunction

define \,\/2(/* X, Y */) with_nargs 2;
    lvars X;
    prolog_push_continuation(/* Y, */ "call\/1", 2);
    prolog_deref(/* X */) -> X;
    fast_chain(prolog_args_nd(X), predicate_valof(prolog_termspec(X)));
enddefine;

;;; ;/2
;;;     disjunction (or conditional if first arg is of the form: P -> X)

define \;\/2(X, Y);
    lvars X, Y;

    define lconstant cond(P, X, Y) with_props \-\>\;\/3;
        lvars P, X, Y;
        SAVE;
        prolog_push_continuation("prolog_own_exit_on_success", 1);
        prolog_deref(P) -> P;
        if prolog_own_invoke(prolog_args_nd(P),
            predicate_valof(prolog_termspec(P)))
        then
            prolog_deref(X) -> X;
            fast_chain(prolog_args(X), predicate_valof(prolog_termspec(X)));
        else
            RESTORE;
            prolog_deref(Y) -> Y;
            fast_chain(prolog_args(Y), predicate_valof(prolog_termspec(Y)));
        endif;
    enddefine;

    if prolog_checkspec(prolog_deref(X) ->> X, "->", 2) then
        chain(prolog_args_nd(X), Y, cond);
    endif;
    SAVE;
    fast_apply(prolog_args_nd(X), predicate_valof(prolog_termspec(X)));
    RESTORE;
    prolog_deref(Y) -> Y;
    fast_chain(prolog_args_nd(Y), predicate_valof(prolog_termspec(Y)));
enddefine;

;;; !/0:
;;;     dynamic cut (i.e. not compiled in) -- not allowed

define \!\/0();
    mishap(0, 'ILLEGAL USE OF CUT');
enddefine;

endsection;     /* prolog */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 15 1993
        Now just defines the most general form of the primitive control
        predicates: the non-checking versions have been moved to
        "optcontrol.p" and the Prolog definitions to "basicpreds.p".
        Abandoned the use of define-forms for predicates.
--- Simon Nichols, May 22 1992
        Changed call/1 to -fast_chain- rather than -chain- a goal.
--- Simon Nichols, Apr  7 1992
        Added call/2, call/3 and support for creating call/N (for N > 3) as
        needed.
--- Robert Duncan, Jan 27 1992
        Removed call of -prologterm*_apply- from -prolog_or- because it
        creates an unnecessary stack frame.
--- Robert John Duncan, Jul  5 1991
        Fixed run-time version of '->;/3' to use -prolog_own_invoke-
        and hence do proper unbinding of variables on backtracking.
--- Robert John Duncan, Jun 24 1991
        Renamed define forms.
--- Rob Duncan, Aug  8 1989
    Sectionised and added #_INCLUDEs for POPC; changed definitions to use
    new define form names: predicate, inline, optimisable.
--- Andrew Casson, Aug  8 1989
    Added definition of ->/2 so that the "else" part is optional
--- Rob Duncan, Sep  5 1988
    Rewrote to make use of -prolog_pdr- and -prolog_inline_pdr-
 */
