/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/predicate_valof.p
 > Purpose:         Prolog: run-time values for predicates
 > Author:          Robert John Duncan, May 13 1993 (see revisions)
 > Documentation:
 > Related Files:
 */


section prolog;

constant procedure ( alias, unwrapped, );

;;; ========================================================================

;;; undefined\/0:
;;;     provides a dummy definition for predicates which have been
;;;     referenced and not yet defined or which have been abolished.
;;;     NB: must have its pdprops set, so that it can be recognised by
;;;     the mishap handler.

define undefined\/0(fn, arity) with_props undefined\/0;
    lvars fn, arity;
    mishap(prolog_maketerm(fn, arity), 1, 'UNDEFINED PREDICATE');
enddefine;

;;; cons_undefined_closure:
;;;     creates a dummy value for undefined predicates

define cons_undefined_closure(fn, arity) -> clos;
    lvars fn, arity, clos;
    undefined\/0(% fn, arity %) -> clos;
    alias(fn, arity) -> pdprops(clos);
enddefine;

;;; is_undefined_closure:
;;;     recogniser for undefined closures (NB: argument assumed to be a
;;;     procedure)

define is_undefined_closure(p);
    lvars p;
    pdpart(p) == undefined\/0;
enddefine;

;;; call\/N:
;;;     implements meta-call -- call/N -- for any value of N > 0

constant procedure ( predicate_valof );     ;;; forward

define call\/N(/* goal, arg1, arg2, ..., argN, argv, */ n);
    lvars goal, argv, n;
    fill(/* argv */) -> argv;
    prolog_deref(/* goal */) -> goal;
    ;;; push goal args
    prolog_args_nd(goal);
    ;;; then the extras
    destvector(argv) -> ;
    ;;; clear the arg vector to prevent garbage
    fill(fast_repeat n times 0 endrepeat, argv) -> ;
    ;;; make the call
    fast_chain(predicate_valof(prolog_termspec(goal) fi_+ n));
enddefine;

;;; cons_call_closure:
;;;     create a procedure value for predicate call/N

define cons_call_closure(arity) -> clos;
    lvars arity, clos;
    call\/N(% writeable initv(arity - 1), arity - 1 %) -> clos;
    alias("call", arity) -> pdprops(clos);
enddefine;

;;; is_special_functor:
;;;     true if the functor fn/arity is "special", with a magic definition;
;;;     currently only the case for call/N (N > 0)

define is_special_functor(fn, arity);
    lvars fn, arity;
    fn == "call" and arity fi_> 0;
enddefine;

;;; cons_special_closure:
;;;     create a (closure) value for a "special" predicate

define cons_special_closure(fn, arity) -> clos;
    lvars fn, arity, clos;
    if fn == "call" then
        cons_call_closure(arity) -> clos;
    else
        mishap(alias(fn,arity), 1, 'NOT A SPECIAL FUNCTOR');
    endif;
enddefine;

;;; predicate_isdefined:
;;;     returns the true procedure value of the predicate fn/arity in
;;;     the current section, stripped of any wrappers, or <false> if
;;;     it's undefined

define predicate_isdefined(fn, arity) -> proc;
    lvars fn, arity, proc;
    unless (isdefined(alias(fn, arity)) ->> proc)
    and isprocedure(idval(proc) ->> proc)
    and not(isundef(proc))
    and not(is_undefined_closure(unwrapped(proc) ->> proc))
    then
        is_special_functor(fn, arity)
        and cons_special_closure(fn, arity) -> proc;
    endunless
enddefine;

;;; predicate_valof:
;;;     returns the raw procedure value associated with predicate
;;;     fn/arity, or a default (undefined) value if it's undefined. This
;;;     is for run-time use -- i.e. where the predicate is about to be
;;;     invoked as a goal -- so it doesn't strip wrappers and checks
;;;     that the predicate spec is valid.

define predicate_valof(fn, arity) -> proc;
    lvars fn, arity, proc;
    if isprologvar(fn) then
        mishap(fn, 1, 'UNINSTANTIATED GOAL');
    elseif isnumber(fn) then
        mishap(fn, 1, 'ATTEMPT TO CALL NUMBER AS GOAL');
    endif;
    unless (isdefined(alias(fn, arity)) ->> proc)
    and isprocedure(idval(proc) ->> proc)
    and not(isundef(proc))
    then
        if is_special_functor(fn, arity) then
            cons_special_closure(fn, arity);
        else
            cons_undefined_closure(fn, arity);
        endif -> proc;
    endunless
enddefine;

endsection;     /* prolog */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug 16 1993
        Exported call/N for poplink.
 */
