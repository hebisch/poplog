/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:           C.all/lib/lib/plog_goals.p
 > Purpose:        Define a PLOG_GOAL record, comprising term and bindings
 > Author:         Roger Evans, May 1984; John Williams, Apr 30 1987 (see revisions)
 > Documentation:  HELP * PLOG_GOALS
 > Related Files:  LIB * PLOGINPOP, C.all/lisp/modules/prolog.lsp
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

uses prolog;
uses define_prolog;

defclass plog_goal
    {   plog_goal_term,
        plog_goal_env,
        plog_goal_process
    };


global vars
    current_plog_goal_env,
    procedure (plog_goal_fail_hook = identfn),
    ;


/* Applying plog_goal structures */

define :prolog global Fail_plog_goal/0() with_nargs 1;
    /* continuation */ ->;
    plog_goal_fail_hook();
enddefine;


define :prolog global Kill_plog_goal/0() with_nargs 1;
    /* continuation */ ->;
    ksuspend(false, stacklength());
enddefine;


define global prolog_barrier_invoke(term);
    prolog_barrier_apply(term, prolog_invoke) ->;
enddefine;


define global plog_goal_invoke(goal, plog_goal_fail_hook);
    lvars process, term;
    dlocal current_plog_goal_env, plog_goal_fail_hook;

    destplog_goal(goal) -> process -> current_plog_goal_env -> term;

    unless process do
        /* Construct a term of the form
            term, Fail_plog_goal ; Kill_plog_goal
        */
        consproc(0, prolog_barrier_invoke(%
                        consprologterm(
                            consprologterm(term, "Fail_plog_goal", ",", 2),
                            "Kill_plog_goal", ";", 2) %))
            ->> process -> plog_goal_process(goal);
    endunless;

    if runproc(0, process) then
        true
    else
        false ->> plog_goal_process(goal)
        ;;; allow exhausted process to be garbage collected
    endif
enddefine;


define global plog_goal_env_bind();
    lvars env, item;
    current_plog_goal_env -> env;
    until null(env) do
        fast_destpair(env) -> env -> item;
        unless null(env) do
            prolog_full_deref(fast_destpair(env) -> env);
            if isident(item) then
                -> idval(item)
            else
                -> valof(item)
            endif
        endunless
    enduntil
enddefine;


define apply_plog_goal() with_nargs 1;
    plog_goal_invoke(
        procedure();
            plog_goal_env_bind();
            suspend(true, 1);
        endprocedure)
enddefine;

apply_plog_goal -> class_apply(plog_goal_key);


global vars plog_goals = true;      ;;; for -uses-

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jun 21 1993
        Revised to use :prolog define form, and defclass.
--- John Williams, Aug 21 1989
        'Kill_plog_goal/0' now returns entire process stack contents
        when doing -ksuspend- (cf FR 4284)
--- John Williams, Feb  1 1989
        Added 'vars plog_goals = true' to placate -uses-
--- John Williams, Jun 24 1988
        Made 'Fail_plog_goal/0' and 'Kill_plog_goal/0' global
 */
