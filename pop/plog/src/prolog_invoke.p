/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/plog/src/prolog_invoke.p
 > Purpose:         Prolog: execute a term as a goal
 > Author:          Robert Duncan, Apr 26 1993 (see revisions)
 */


section prolog;

constant
    procedure ( see, tell, predicate_valof, ),
;

weak constant
    procedure ( prolog_exception_final, prolog_pr_exception, ),
;

vars
    procedure ( compilerin, )
    current_output,
;

weak vars
    prolog_exceptions, prolog_read_prompt,
    oride, skip, push, invoc,
        ;;; debugger variables -- see "spy.p"
;

;;; ========================================================================

vars
    procedure prolog_toplevel_trap = identfn,
        ;;; user hook
;

;;; invoke, exit_on_success:
;;;     low-level invocation mechanism: -invoke- calls the procedure
;;;     corresponding to the executed goal and returns <false> if it
;;;     returns; -exit_on_success- is the continuation, which aborts the
;;;     call and returns <true> on success. Invocation should always be
;;;     inside a Prolog barrier.

constant exit_on_success_id =
    word_identifier("exit_on_success", current_section, "undef");

define lconstant invoke(goal) with_props false;
    lvars goal;
    prolog_push_continuation(exit_on_success_id, 1);
    fast_apply(prolog_args_nd(goal), predicate_valof(prolog_termspec(goal)));
    false;
enddefine;

define exit_on_success();
    exitfrom(true, invoke);
enddefine;

;;; prolog_sysinvoke:
;;;     reset state variables and invoke a goal within a Prolog barrier

define prolog_sysinvoke(/* goal */) with_props prolog_invoke with_nargs 1;
    dlocal
        weakref prolog_read_prompt = '|: ',
        weakref oride = "no",
        weakref push = "any",
        weakref skip = 0,
        weakref invoc = 0,
    ;
    if testdef prolog_exceptions then
        dlocal
            weakref prolog_exceptions = true,
            pop_exception_final = weakref prolog_exception_final,
            pop_pr_exception = weakref prolog_pr_exception;
    endif;
    prolog_var_number(false);
    prolog_toplevel_trap();
    prolog_barrier_apply(/* goal, */ invoke);
enddefine;

;;; prolog_safeinvoke:
;;;     wrapper for prolog_sysinvoke when called from a non-Prolog
;;;     environment: ensures the I/O streams are set correctly

define prolog_safeinvoke() with_props prolog_invoke with_nargs 1;
    dlocal compilerin = cucharin, cucharin, proglist;
    see("user"); unless current_output then tell("user") endunless;
    prolog_sysinvoke();
enddefine;

;;; prolog_invoke:
;;;     execute a term as a goal

define prolog_invoke() with_nargs 1;
    lvars name;
    if (sys_compiler_subsystem(`c`) ->> name) == "top" or name == "prolog"
    then
        chain(prolog_sysinvoke);
    else
        chain(prolog_safeinvoke);
    endif;
enddefine;

endsection;     /* prolog */


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, May 13 1996
        Changes for new exception handling
 */
