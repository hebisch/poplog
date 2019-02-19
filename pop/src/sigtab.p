/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/sigtab.p
 > Purpose:         Definition of initial signal table
 > Author:          Roger Evans, Jan  21 1988 (see revisions)
 > Documentation:   REF SIGNALS
 > Related Files:   asignals.s signals.ph signals.p
 */

#_INCLUDE 'declare.ph';
#_INCLUDE 'signals.ph';


global constant
    procedure (Sys$-Bad_err_interrupt, Sys$-Abnormal_sysexit)
    ;

global vars
    pop_exit_ok
    ;

;;; ---------------------------------------------------------------------

    /* default standard handlers */

vars procedure (
    interrupt       = sysexit,
    timer_interrupt = identfn,
    io_interrupt    = identfn,
    );

define vars keyboard_interrupt();
    interrupt()
enddefine;


section $-Sys;

    ;;; default table values
vars
    Sig_default_flag = true;        ;;; default is enabled, block self only

    ;;; default handler mishaps
define Sig_default_proc(signum);
    lvars signum;
    dlocal interrupt = Bad_err_interrupt(interrupt);
    mishap(signum, 1, 'NO HANDLER FOR SIGNAL');
enddefine;


    ;;; the tables
vars
    Sys_signal_procs = writeable
        consvector(#|repeat NUM_SYS_SIGS times Sig_default_proc endrepeat|#),

    Sys_signal_flags = writeable
        consvector(#|repeat NUM_SYS_SIGS times Sig_default_flag endrepeat|#),

    Sys_max_signal   = NUM_SYS_SIGS,
;

    ;;; non-default initialisation

#_IF DEF SIG_INT
    ident keyboard_interrupt -> fast_subscrv(SIG_INT,Sys_signal_procs);
    {} -> fast_subscrv(SIG_INT,Sys_signal_flags);   ;;; don't block self
#_ENDIF;

#_IF DEF SIG_ALRM
    ident timer_interrupt -> fast_subscrv(SIG_ALRM,Sys_signal_procs);
#_ENDIF;

#_IF DEF SIG_IO
    ident io_interrupt -> fast_subscrv(SIG_IO,Sys_signal_procs);
#_ENDIF

#_IF DEF SIG_HUP
    ;;; exiting abnormally - but with VED tidying etc.
    Abnormal_sysexit -> fast_subscrv(SIG_HUP,Sys_signal_procs);
#_ENDIF;

#_IF DEF SIG_TERM
    ;;; exiting abnormally - but with VED tidying etc.
    Abnormal_sysexit -> fast_subscrv(SIG_TERM,Sys_signal_procs);
#_ENDIF;

#_IF DEF SIG_PIPE
    ;;; ignore pipe
    false -> fast_subscrv(SIG_PIPE,Sys_signal_procs);
#_ENDIF;

#_IF DEF SIG_CONT
    ;;; ignore cont
    false -> fast_subscrv(SIG_CONT,Sys_signal_procs);
#_ENDIF

#_IF DEF SIG_TSTP
    ;;; handler defined in sig_stop.p
    Sys_stop_handler -> fast_subscrv(SIG_TSTP,Sys_signal_procs);
#_ENDIF

#_IF DEF SIG_TTIN
    ;;; handler defined in sig_stop.p
    Sys_stop_handler -> fast_subscrv(SIG_TTIN,Sys_signal_procs);
#_ENDIF

#_IF DEF SIG_TTOU
    ;;; use unix stop for TTOU, cos we can't restart writes properly
    ;;; after a call of Sys_stop_handler  RE.
    true -> fast_subscrv(SIG_TTOU,Sys_signal_procs);
#_ENDIF

#_IF DEF SIG_CHILD
    ;;; ignore child (SIG_DFL means ignore, but without the side-effects of
    ;;; SIG_IGN)
    true -> fast_subscrv(SIG_CHILD,Sys_signal_procs);
#_ENDIF

#_IF DEF SIG_WINCH
    ;;; ignore winch
    false -> fast_subscrv(SIG_WINCH,Sys_signal_procs);
#_ENDIF

#_IF DEF SIG_XCPU
    mishap(%0,'CPU TIME LIMIT EXCEEDED'%)
            -> fast_subscrv(SIG_XCPU,Sys_signal_procs);
#_ENDIF

#_IF DEF SIG_XFSZ
    ;;; ignore file size limit exceeded
    false -> fast_subscrv(SIG_XFSZ,Sys_signal_procs);
#_ENDIF


endsection ;;; $-Sys



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 16 1996
        Added keyboard_interrupt for SIG_INT
--- John Gibson, Oct 26 1990
        Added actions for SIG_XCPU and SIG_XFSZ
--- John Gibson, Sep 21 1990
        Added assignment of {} to Sys_signals_flags for SIG_INT
--- John Gibson, Sep 10 1990
        Changed Set_se*tpop_in*terrupt- to -Bad_err_interrupt-, tidied
        up signal vectors.
--- Rob Duncan, Jun 28 1990
        Changed handler for SIG_CHILD to SIG_DFL, which means "ignore" on
        all systems.
--- John Gibson, Aug 25 1989
        Replaced sysint SIG_ macros with pop ones.
--- John Gibson, Aug 24 1989
        Removed #_IF for S*IGNALS
--- John Gibson, Aug 22 1989
        Replaced lconstant -Ab_exit- with Sys$-Abnormal_sysexit
--- John Gibson, Aug 15 1989
        Made -Sig_default_proc- a constant. Made -interrupt- dlocal to
        it (so -Set_setpop_interrupt- doesn't change interrupt globally)
--- Roger Evans, Jun 15 1989
        TTOU handler now unix default (cos its not easy to restart
        an interrupted write)
--- Roger Evans, Nov 18 1988
        CONT handler functionality moved into stop handler.
        CONT is now ignored.
--- Roger Evans, Nov 17 1988
        changed HUP and TERM handers to do tidying non-outputting exits
--- Roger Evans, Oct 12 1988
        Added initialisers for TSTP and CONT, changed TTIN and TTOU to conform
--- Roger Evans, Oct  7 1988
        Added initialisation for TTIN and TTOU
--- Roger Evans, Sep 26 1988
        Changed SIG_DEFAULT_FLAG/PROC to Sig_default_flag/proc to overcome
        VMS symbol name problems. Rearranged again!
--- Roger Evans, Jun 17 1988
        changed format so that assignments depend on presence of signal
        macro rather than machine-type macro - now all machine/signal
        dependencies can be handled in signals.ph
--- Roger Evans, Apr 19 1988
        moved non-default assignments to SIG_IO and SIG_CHILD from
        #_IF DEF SUN (etc.) to #_IF DEF BERKELEY
--- Roger Evans, Apr 18 1988
        moved code into section Sys
--- Roger Evans, Apr 11 1988
        added initialisations for ORION
--- Roger Evans, Mar 25 1988
        added initialisations for BOBCAT
--- Roger Evans, Feb 23 1988
        Installed in system masters
 */
