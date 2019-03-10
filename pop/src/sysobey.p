/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.unix/src/sysobey.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSUTIL
 */

;;; ------------------ RUN A SHELL COMMAND -------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'signals.ph'

constant
        procedure (sys_vfork, sys_wait, sysexecute, sys_signal_handler,
        Sys$-Io$-Set_term_state
        ),
    ;

vars
        pop_status
;

;;; ---------------------------------------------------------------------

section $-Sys => sysobey;

    ;;; run an executable file as a child process and wait for it
define sysobey(args);
    lvars execfile, args, arg0, may_do_term_output = true;
    if isboolean(args) then
        ;;; allows false for -may_do_term_output-, which stops the
        ;;; -Set_term_state- below (which if done will cause a terminal
        ;;; prompt to be re-output).
        ((), args) -> (args, may_do_term_output)
    endif;
    if islist(args) then
        -> execfile
    else
        '/bin/sh' ->> execfile -> arg0;
        if isinteger(args) then
            if args == `%` then
                '/bin/csh' ->> execfile -> arg0
            elseif args == `!` then
                systranslate('SHELL') ->> execfile -> arg0
            endif;
            -> args
        endif;
        if args = '' then
            [^arg0 '-i']  ;;; sakw: make shell interactive
        else
            [^arg0 '-c' ^args]
        endif -> args
    endif;

    applist(args, Check_string);

    unless On_line_term(weakref popdevin) then
        false -> may_do_term_output
    endunless;
    if may_do_term_output then
        Io$-Set_term_state(weakref popdevin, false)
    endif;

    procedure();
        lvars child_pid;
        dlocal  % sys_signal_handler(SIG_QUIT) %;

        if sys_vfork(true) ->> child_pid then
            if ispair(args) and ispair(fast_back(args))
            and fast_front(fast_back(args)) = '-i'
            then
                ;;; sakw: ignore quits
                false -> sys_signal_handler(SIG_QUIT)
            endif;
            if may_do_term_output then
                dlocal interrupt = identfn;
            endif;
            sys_wait(child_pid) -> (, pop_status)
        else
            sysexecute(execfile, args, false)
        endif
    endprocedure()
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, May 27 1994
        Locally set interrupt to identfn in parent if may_do_term_output
        is true.
--- John Gibson, Apr 19 1994
        Uses new sys_vfork/sys_wait
--- John Gibson, Dec  8 1992
        Removed dlocaling of SIG_CHILD (no longer needed)
--- John Gibson, Jun 20 1991
        Allowed optional bool arg to say whether terminal output may
        be produced or not.
--- John Gibson, Oct 26 1990
        Uses -On_line_term-
--- Robert John Duncan, Jun 29 1990
        Changed signal handling to avoid special case for System V type
        systems: handler for SIG_CHILD is made SIG_DFL for duration of
        both "wait" and "exec".
        Replaced calls to "_extern signal" with -sys_signal_handler-.
--- Simon Nichols, Jun 18 1990
        Added test for not(DEF RISCOS) when changing SIG_CHILD locally
--- John Gibson, Aug 28 1989
        -Set_term_state- takes extra arg
--- John Gibson, Aug 25 1989
        Uses _:SIG_ instead of sysint versions
--- John Gibson, Aug 22 1989
        Added test for not(DEF HPUX) when changing SIG_CHILD locally
--- John Gibson, Aug 15 1989
        Changed test for terminal input to use -On_line_tty_input-
--- Roger Evans, Apr 18 1988
        Moved _signal_handler to section Sys
--- John Gibson, Mar 17 1988
        Moved out of sysutil.p
 */
