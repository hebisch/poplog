/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/optcontrol.p
 > Purpose:         Prolog: non-checking versions of control predicates
 > Author:          Robert Duncan, Apr 29 1993
 > Documentation:
 > Related Files:
 */


section prolog;

/*
 *  Non-checking versions of control predicates called from compiled code
 *  ONLY. They can assume that their arguments are always Prolog terms and
 *  that the corresponding predicate procedures are defined.
 */

define prolog_conjunction\/2(X, Y) with_props \,\/2;
    lvars X, Y;
    prolog_push_continuation(destprologterm(Y) fi_+ 1);
    chain(prologterm_dest_valof(X));
enddefine;

define prolog_disjunction\/2(X, Y) with_props \;\/2;
    lvars X, Y;
    SAVE;
    prologterm_dest_valof(X)();
    RESTORE;
    chain(prologterm_dest_valof(Y));
enddefine;

define prolog_conditional\/3(P, X, Y) with_props \-\>\;\/3;
    lvars P, X, Y;
    SAVE;
    prolog_push_continuation("prolog_own_exit_on_success", 1);
    if prolog_own_invoke(prologterm_dest_valof(P)) then
        chain(prologterm_dest_valof(X))
    else
        RESTORE;
        chain(prologterm_dest_valof(Y));
    endif;
enddefine;

define prolog_cut\/1(/* stacklen */) with_props \!\/0 with_nargs 1;
    chainto(
        /* stacklen */ fi_+ callstacklength(prolog_barrier_apply),
        prolog_apply_continuation)
enddefine;

define prolog_fail_if\/1(G) with_props \\\+\/1;
    lvars G;
    SAVE;
    prolog_push_continuation("prolog_own_exit_on_success", 1);
    unless prolog_own_invoke(prologterm_dest_valof(G)) then
        RESTORE;
        chain(prolog_apply_continuation);
    endunless;
enddefine;

endsection;     /* prolog */
