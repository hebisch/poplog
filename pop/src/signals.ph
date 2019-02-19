/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/signals.ph
 > Purpose:         symbol definitions for signals
 > Author:          Roger Evans, Jan  8 1988 (see revisions)
 > Documentation:   REF signals
 > Related Files:   signals.p sigtab.p
 */

;;;---------------------SIGNAL DEFINITIONS----------------------------

global vars procedure (interrupt, timer_interrupt);

global vars procedure (io_interrupt, popsuspend);

section $-Sys;

global constant
        procedure (Sig_default_proc, Sys_cont_handler, Sys_stop_handler,
        Check_astp_arg),
        Sys_ast_queue
    ;

global vars
        Sys_signal_procs, Sys_signal_flags, Sys_max_signal,
        Sys_ast_queue_end, Sig_default_flag,
        _ignore_ast_queue, _ast_queue_flags
    ;


endsection;


;;; ------------------------------------------------------------------

#_IF DEF UNIX

    ;;; symbols for built-in Unix handlers
lconstant macro (
    SIG_DFL     = 0,       ;;; actually (int (*func) ()) 0
    SIG_IGN     = 1,       ;;; actually (int (*func) ()) 1
);

#_ENDIF


;;; ------------------------------------------------------------------

;;; Signal numbers are defined centrally in lib/include/sigdefs.ph for use by
;;; system and users.
#_INCLUDE 'io.ph'
#_INCLUDE '../lib/include/sigdefs.ph'
#_INCLUDE '../lib/include/ast.ph'

lconstant macro (
    ;;; number of system-defined signals
    NUM_SYS_SIGS    = MAXSIG,

    ;;; Flags in _ast_queue_flags
    QC_RECURSIVE    = 2:1e0,    ;;; blocked AST because of recursion
    QC_CALLBACK     = 2:1e1,    ;;; blocked AST because of callback

    ;;; ASynchronous Trap types (must agree with c_core.h)
    AST_SIGNAL      = 0,        ;;; Unix-style signal
    AST_TIMER       = 1,        ;;; Timer
    AST_APP_PENDING = 2,        ;;; Events pending for Xt appcontext
    AST_QUEUE_CHECK = 3,        ;;; Dummy to check AST queue
    AST_HANDLE      = 4,        ;;; Win32 HANDLE signaled
    AST_DEV         = 5,
    AST_DEV_READ    = AST_DEV+RD_SET,   ;;; Input available on device
    AST_DEV_WRITE   = AST_DEV+WR_SET,   ;;; Output possible on device
    AST_DEV_EXCEPT  = AST_DEV+EX_SET,   ;;; Exception condition on device


    ;;; Subscripts for entries in os_process_list
    OSPROC_PID      = 1,
    OSPROC_FLAGS    = 2,
    OSPROC_STATUS   = 3,
    OSPROC_ASTP     = 4,
    OSPROC_VECLEN   = 4,
    ;;; Flags in OSPROC_FLAGS
    OSPROCF_WILL_WAIT   = 2:1e0,
    OSPROCF_DEAD        = 2:1e1,
    );



/* --- Revision History ---------------------------------------------------
--- Robert Duncan, May 24 1996
        Added AST_HANDLE for Win32
--- John Gibson, Apr  6 1994
        Added new flags
--- John Gibson, Mar 20 1991
        Added AST_APP_PENDING
--- John Gibson, Jan  3 1991
        Removed S*IGNAL
--- John Gibson, Dec  6 1990
        Made include for sigdefs.ph use relative pathname
--- John Gibson, Dec  3 1990
        Added AST_ macros
--- John Gibson, Sep 10 1990
        Changed SIG_NOTREENTRANT to SIG_RE_ENTRANT = `R`
--- Jonathan Meyer, Jul 10 1990
        Added IF DEF ATT386 and SIGNAL macro.
--- John Gibson, Aug 25 1989
        Got rid of all sysint signal numbers ( _: in front of pop one can
        be used instead).
--- John Gibson, Aug 24 1989
        Removed #_IFs for S*IGNALS
--- John Gibson, Aug 15 1989
        Made -Sig_default_proc- a constant.
--- John Gibson, Feb 20 1989
        Replaced 'uses sigdefs' with #_INCLUDE lib/include/sigdefs.ph
--- Roger Evans, Oct 12 1988
        Added decalrations for STOP/CONT code
--- Roger Evans, Sep 26 1988
        Changed SIG_DEFAULT_PROC/FLAG to Sig_Default_proc/flag to
        cope with VMS symbols problem. Added vms definitions.
        Changed to use LIB SIGDEFS, hence centralising signal definitions
        for system and users
--- Rob Duncan, Sep  5 1988 added Symmetry definitions
--- Roger Evans, Apr 18 1988 added $-Sys declarations
--- Roger Evans, Apr 11 1988 moved SIG_PROF into standard Berkeley settings
--- Roger Evans, Apr  8 1988 added orion definitions
--- Roger Evans, Mar 25 1988 added bobcat definitions
--- Roger Evans, Feb 23 1988 Installed in system masters
 */
