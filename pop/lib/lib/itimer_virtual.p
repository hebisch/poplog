/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.unix/lib/lib/itimer_virtual.p
 > Purpose:         Old fixed virtual timer
 > Author:          John Williams, Feb  7 1990 (see revisions)
 > Documentation:   HELP * ITIMER_VIRTUAL
 > Related Files:   C.unix/src/systimer.p
 */
compile_mode:pop11 +strict;

section;

/* Set up handler for (Poplog-internal) VTALRM signal */

include sigdefs.ph;

global vars procedure vtimer_interrupt = identfn;

define lconstant Raise_Sigvtalrm =
    sys_raise_signal(% SIG_VTALRM %)
enddefine;

/* Set timer */
define global syssetvtimer(hsecs);
    lvars hsecs;
    if isprocedure(hsecs) then
        ((), hsecs) -> (hsecs, vtimer_interrupt)
    endif;
    hsecs * 1e4 -> sys_timer(Raise_Sigvtalrm, 1)    ;;; 1 = virtual
enddefine;

/* Cancel timer */
define global syscanvtimer =
    updater(sys_timer)(% false, Raise_Sigvtalrm %)
enddefine;

sys_runtime_apply(
        procedure;
            ident vtimer_interrupt -> sys_signal_handler(SIG_VTALRM)
        endprocedure);

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 22 1992
        Added runtime assignment of vtimer_interrupt
--- John Gibson, Jan  7 1991
        Rewritten to use -sys_timer- (essential it doesn't use setitimer
        anymore).
--- Robert John Duncan, Nov 26 1990
        Revised to use new exload/exacc syntax.
--- John Williams, Apr 26 1990
        Made -syscanvtimer- global.
 */
