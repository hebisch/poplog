/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.unix/src/sig_stop.p
 > Purpose:         Handlers for STOP and CONT signals
 > Author:          Roger Evans, Oct 11 1988 (see revisions)
 > Documentation:   REF SIGNALS
 > Related Files:   sigtab.p vd_stop.p
 */

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'
#_INCLUDE 'unixdefs.ph'
#_INCLUDE 'signals.ph'


    /* File empty if none of these defined */
#_IF DEF SIG_TSTP or DEF SIG_TTIN or DEF SIG_TTOU

global constant
        procedure (sys_signal_handler, sys_send_signal, syshibernate,
        Sys$-Io$-Restore_tty_devs, Sys$-Io$-Zap_tty_devs),
    ;

global vars
        poppid
    ;


weak global constant
        procedure (vedprocess, Sys$-Ved$-Stop_handler)
    ;


;;; ----------------------------------------------------------------

section $-Sys => popsuspend;

;;; user hook called during TSTP/CONT operation - arg is signal number
;;; before supsension, false after restart
global vars procedure popsuspend = erase;


;;; --- Handler for TSTP TTIN TTOU ------------------------------

;;; TSTP TTIN and TTOU all result in a stop (so does STOP, but we can't
;;; trap it). TSTP is voluntary, and so sets the terminal in
;;; a sensible state before stopping. TTIN and TTOU are involuntary, and
;;; any attempt at terminal control may hang, so we leave it.

define Sys_stop_handler(sig);
    lvars sig;

    popsuspend(sig);                    ;;; call user proc

    ;;; tell VED
    if testdef vedprocess then
        weakref[vedprocess] Ved$-Stop_handler(sig)
    endif;

    ;;; signals need to be disabled before doing the actual suspend, so that
    ;;; Restore_tty_devs and Zap_tty_devs aren't interrupted.
    dlocal pop_asts_enabled = false;

    if sig == SIG_TSTP then
        Io$-Restore_tty_devs()       ;;; restore terminal to entry state
    endif;


    procedure;
        ;;; set signal locally to Unix default action (stop)
        dlocal % sys_signal_handler(sig) % = true;
        ;;; do the signal for real
        sys_send_signal(poppid, sig) -> ;
    endprocedure();

    ;;; --- suspended here ---


    ;;; Zap the ttyparams of all terminals (forcing their initial state
    ;;; to be got again when first used).
    Io$-Zap_tty_devs();

    ;;; mustn't re-enable sigs until AFTER zapping ttypparams
    true -> pop_asts_enabled;

    ;;; so procedures know we're continuing
    fi_negate(sig) -> sig;

    ;;; tell VED
    if testdef vedprocess then
        weakref[vedprocess] Ved$-Stop_handler(sig)
    endif;

    ;;; call user procedure
    popsuspend(sig);
enddefine;

endsection;

#_ENDIF


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  9 1995
        Removed pw*m stuff
--- John Gibson, Dec 11 1992
        Moved PW*M code to Pw*m$-Stop_handler in vdpw*m.p
--- John Gibson, Jun 17 1992
        Moved disabling of signals to after the point where -popsuspend- is
        called.
--- John Gibson, May  8 1992
        After last change, re-enabled signal processing when continuing,
        but not until after tyyparams have been zapped.
--- John Gibson, Jun 10 1991
        Made -pop_asts_enabled- dlocal to -Sys_stop_handler-
--- John Gibson, Aug 24 1990
        Changed call to -s*et_process entry_term- to -Restore_tty_devs-
--- John Gibson, Aug 28 1989
        -Zap_tty_devs- replaces -S*et_std_in_tty-
--- John Gibson, Aug 24 1989
        Moved from C.bsd into C.unix and wrapped all code in #_IF
        testing for presence of appropriate signals.
--- John Gibson, Apr 30 1989
        Ved_stop/cont_handler into section Sys$-Ved.
--- John Gibson, Feb 20 1989
        Included io.ph
            Resetting of standard input tty parameters now done by
        Io$-Set_std_in_tty (Io$-std_in_tty replaces
        Io$-process_entry_ttyparams).
--- John Williams, Dec  7 1988
        Re-installed because not linked into correct directories
--- Roger Evans, Nov 18 1988
        CONT handler functionality moved into stop handler.
        CONT is now ignored.
--- Roger Evans, Nov 17 1988
        Added CHECKINTERRUPT to CONT handler
--- Roger Evans, Nov 17 1988
        moved to C.bsd (instead of C.unix, which was wrong)
--- Roger Evans, Nov  2 1988
        now checks process_entry_ttyparams is non-false before setting params
--- Roger Evans, Oct 12 1988
        Changed to pass -sig to popsuspend on cont, and to attempt
        PW*M tidy for TSTP
 */
