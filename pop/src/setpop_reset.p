/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/setpop_reset.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; ----------------- RESET THINGS IN setpop ----------------------------
;;;   N.B. Only setpop_reset should be initialised in this file

#_INCLUDE 'declare.ph'
#_INCLUDE 'process.ph'

global constant
        procedure (Sys$-Dealloc_callstack_mem)
    ;

weak global constant
        procedure (prolog_reset, syscantimer),
        active (pop_default_section)
    ;

global vars
        pop_first_setpop
    ;

weak global vars
        procedure pop_setpop_compiler,
        Sys$-userstack_stack
    ;


;;; ---------------------------------------------------------------------

section $-Sys;

    /*  Invoked by setpop, and Vedcompile and vedsetpop (in vddoit.p)
    */
define setpop_reset(top_level, restart_compilation);
    lvars top_level, restart_compilation, t;

    Dealloc_callstack_mem();
    clearstack();
    Clear_ast_queue(false);

    unless pop_first_setpop then
        unless testdef pop_charin_device
        and (systrmdev(weakref pop_charin_device) ->> t) then
            sysexit()
        endunless;

        sysflush(pop_charout_device);
        if t == true then
            weakref[pop_charin_device]
                sys_clear_input(weakref pop_charin_device);
            `\n` -> poplastchar
        ;;; else don't try to clear tty input if in background
        endif
    endunless;

    if top_level then
        ;;; reset prolog area (just to be safe)
        if testdef prolog_reset then weakref prolog_reset() endif;

        ;;; clear process userstack stack (just to be safe)
        if testdef process_key then
            weakref[process_key] userstack_stack@STK_DATA
                        -> weakref[process_key] userstack_stack!STK_PTR
        endif
    endif;

    if testdef syscantimer then weakref syscantimer() endif;

    if testdef current_section then
        weakref[current_section] pop_default_section -> weakref current_section
    endif;

    _0 -> _disable;                 ;;; re-enable signal raising
    false -> weakref popnewline;
    0 ->> pop_charout_col -> pop_charerr_col;
    charout -> cucharout;
    charerr -> cucharerr;

    false -> pop_first_setpop;

    if testdef pop_setpop_compiler and restart_compilation then
        chain(weakref pop_setpop_compiler)
    endif
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  9 1995
        Removed pw*m stuff
--- John Gibson, Apr 12 1994
        Added call of Clear_ast_queue in setpop_reset
--- John Gibson, Jun  8 1991
        wved_ rationalisation
--- John Gibson, Mar 14 1991
        Removed call of C*lear_signals
--- John Gibson, Oct 27 1990
        Rewrote -setpop_reset-
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Sep 11 1989
        -setpop_reset- now called by ved setpop procedures as well as
        -setpop- itself.
--- John Gibson, Aug 16 1989
        Undid last change, but stopped clearing of input on pop_charin_device
        if a terminal and job in background
--- John Gibson, Aug 15 1989
        Changed test for terminal input to use -On_line_tty_input-
--- John Gibson, Apr  9 1988
        Moved out of setpop.p
 */
