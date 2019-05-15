/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/plog/lib/on_interrupt.pl
 > Purpose:         Handling keyboard interrupts
 > Author:          Simon Nichols, Jun 19 1991
 > Documentation:   HELP * ON_INTERRUPT
 */

:- system_predicate on_interrupt/2.

:- module prolog.

:- prolog_language(pop11).

compile_mode:pop11 +strict;

12 -> item_chartype(`\\`, readitem);    ;;; alphabeticiser


define on_interrupt\/2(OldAction, NewAction);
    lvars OldAction, NewAction, old_action;

    define lconstant invoke(/* Goal */) with_nargs 1;
        prolog_sysinvoke(prolog_instance(/* Goal */)) -> ;
    enddefine;

    if prolog_interrupt == identfn then
        "abort";
    elseif pdpart(prolog_interrupt) == invoke then
        prolog_instance(frozval(1, prolog_interrupt));
    else
        prolog_interrupt;
    endif -> old_action;

    returnunless(prolog_unify(OldAction, old_action));

    prolog_deref(NewAction) -> NewAction;
    if isprologvar(NewAction) then
        prolog_assign(NewAction, old_action);
    elseif NewAction == "abort" then
        identfn -> prolog_interrupt;
    elseif isprocedure(NewAction) then
        NewAction -> prolog_interrupt;
    else
        invoke(% prolog_generalise(NewAction) %) -> prolog_interrupt;
    endif;
    chain(prolog_apply_continuation)
enddefine;

:- prolog_language(prolog).

:- endmodule prolog.
