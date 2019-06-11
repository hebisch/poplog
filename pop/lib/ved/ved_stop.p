/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 >  File:           C.unix/lib/ved/ved_stop.p
 >  Purpose:        Gracefully suspend current process from within VED.
 >  Author:         Chris Slymon, Aug  1984 (see revisions)
 >  Documentation:  HELP * STOP
 >  Related Files:  SRC *VD_STOP
 */
compile_mode :pop11 +strict;

include sigdefs.ph;

section;

define vars ved_stop;
    sys_send_signal(0, SIG_TSTP) -> ;   ;;; send signal to process group ...
    syshibernate()                      ;;; ... and wait for it
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- John Gibson, Jan 15 1993
        Made it use sys_send_signal to send sig to process group
--- Roger Evans, Oct 12 1988
    Moved all the code into the default TSTP signal handler
--- Aaron Sloman, Jan 29 1988
    Previously did vedscreenlength -> vedstartwindow, and this caused
    problems if was in upper window larger than lower window.
 */
