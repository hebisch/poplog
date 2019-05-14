/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 >  File:           C.unix/lib/auto/stop.p
 >  Purpose:        Gracefully suspend current process from pop
 >  Author:         Ben Rubinstein (see revisions)
 >  Documentation:  HELP * STOP
 >  Related Files:  LIB *VED_STOP SRC *SIG_STOP
 */
compile_mode :pop11 +strict;

include sigdefs.ph;

section;

define vars syntax stop;
    ";" :: proglist -> proglist;
    sys_send_signal(0, SIG_TSTP) -> ;   ;;; send signal to process group ...
    syshibernate();                     ;;; ... and wait for it
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 19 1993
        Made syntax instead of macro
--- John Williams, Mar 19 1993
        Now uses sys_send_signal (as does ved_stop)
--- Roger Evans, Oct 12 1988
        Moved all the code into the default TSTP signal handler
 */
