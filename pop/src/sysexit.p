/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/sysexit.p
 > Purpose:
 > Author:          John Gibson, Aug 22 1989 (see revisions)
 > Documentation:   REF *SYSTEM
 */

;;; ---------------------- SYSTEM EXIT ------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'

global constant
        procedure (external_do_load, set_process_entry_term)
    ;

weak global constant
        procedure (vedprocess, vedpopexit, sys_wait, vedsetpop)
    ;

weak global vars
        procedure (vederror)

    ;

section $-Sys;

constant
        procedure (Close_filetab_files, Opsys_exit, Abnormal_sysexit,
        Extern$-Delete_link_files)
    ;

vars
        _vfork_child
    ;

endsection;


;;; --------------------------------------------------------------------

section $-Sys => pop_exit_ok, xpopexit, sysexit, fast_sysexit;

vars
    pop_exit_ok         = true,
    procedure xpopexit  = identfn,
    ;

define fast_sysexit();
    Opsys_exit(pop_exit_ok)
enddefine;

    ;;; Bad_err_interrupt recognises sysexit interrupt handlers by their
    ;;; pdprops being sysexit
define lconstant Exit_continue = exitto(%sysexit%) enddefine;
sysexit -> pdprops(Exit_continue);

define lconstant Closefile_continue = exitto(%Close_filetab_files%) enddefine;
sysexit -> pdprops(Closefile_continue);


define sysexit();

#_IF DEF UNIX
    if _vfork_child then Opsys_exit(true) endif;
#_ENDIF

    define lconstant flush(dev);
        lvars dev;
        if isdevice(dev) and not(dev!D_FLAGS _bitst _M_D_CLOSED) then
#_IF DEF VMS
            sysclose(dev)
#_ELSE
            sysflush(dev)
#_ENDIF
        endif
    enddefine;

    if iscaller(sysexit, 1) then
        ;;; if already in sysexit, continue with the previous one
        chain(sysexit, exitto)
    endif;

    if testdef sys_wait and iscaller(weakref sys_wait)
    and not(iscaller(Abnormal_sysexit))
    and not(testdef vedsetpop and iscaller(weakref vedsetpop))
    then
        sys_pr_message(0,
            {'CAN\'T EXIT SYSTEM (still waiting for child process)' 16:11},
                    nullstring, `W`);
        exitto(weakref sys_wait)
    endif;

    lvars ttyin = On_line_term(weakref popdevin);
    dlocal interrupt = if ttyin then setpop else Exit_continue endif;

    ;;; Call user exit procedures first (popexit is declared
    ;;; an incremental procedure in declare.ph)
    popexit();

    if VED_LOADED then VED_WEAK vedpopexit() endif;

    if popunderx then xpopexit() endif;

    if testdef external_do_load then
        ;;; delete any files created by external_do_load
        weakref[external_do_load] Extern$-Delete_link_files()
    endif;

    unless ttyin then Closefile_continue -> interrupt endunless;
    Close_filetab_files(true);  ;;; close all files in heap

    ;;; tidy up standard output
    unless ttyin then Exit_continue -> interrupt endunless;

    flush(popdevout);       ;;; flush the output first in case exquota
    Exit_continue -> interrupt;
    flush(popdeverr);
    if testdef poprawdevout then flush(weakref poprawdevout) endif;

#_IF DEF UNIX
    ;;; set tty back to process entry state
    set_process_entry_term();
#_ENDIF

    Opsys_exit(pop_exit_ok) ->
enddefine;

define Abnormal_sysexit();

    define lconstant Assign_interrupt(p);
        lvars p;
        p ->> interrupt ->> pop_exception_handler
            ->> pop_exception_final
            -> VED_WEAK vederror
    enddefine;

    false -> pop_exit_ok;
    _DISABLE_INTERRUPTS -> _disable;
    Assign_interrupt(fast_sysexit);     ;;; for the duration of this procedure

    ;;; Ensure no terminal I/O
    ;;; (VMS translates /dev/null to nla0:)
    lvars null_dev = sysopen('/dev/null', 2, false);
    if popdevout!D_FLAGS _bitst _M_D_INTERACTIVE then
        null_dev -> popdevout
    endif;
    if popdeverr!D_FLAGS _bitst _M_D_INTERACTIVE then
        null_dev -> popdeverr
    endif;
    null_dev ->> weakref popdevin
             ->> weakref poprawdevin -> weakref poprawdevout;

    Assign_interrupt(Exit_continue);
    sysexit()
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb  6 1996
        Uses sys_pr_message instead of sys*prmessage,
        pop_exception_final instead of pr*mishap, etc.
--- John Gibson, Nov  9 1995
        Removed pw*m stuff
--- John Gibson, Feb 24 1995
        Made sysexit interrupt handlers like Exit_continue have sysexit in
        their pdprops so Bad_err_interrupt recognises them.
--- John Gibson, Apr 19 1994
        sys*wait -> sys_wait
--- John Gibson, Jan 25 1994
        Made Abnormal_sysexit assign null_dev to popdevout/err if they are
        _M_D_INTERACTIVE, not just _M_D_TERMINAL
--- John Gibson, Dec  8 1993
        Added check in sysexit for being inside a call of sys_wait
--- John Gibson, Nov 16 1990
        Substituted e*xternal_load with external_do_load
--- Aaron Sloman, Sep 28 1990
        Added call of xpopexit
--- John Gibson, Jan 25 1990
        Made _vfork_child test unconditional in Unix.
--- John Gibson, Aug 29 1989
        In -sysexit-, Made sure -interrupt- is always -Exit_continue-
        when input not from terminal.
--- John Gibson, Aug 23 1989
        Commoned Unix and VMS versions of -sysexit- and moved here from
        sysutil.p
 */
