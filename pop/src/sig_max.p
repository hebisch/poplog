/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/src/sig_max.p
 > Purpose:         routines to control the maximum number of signals
 > Author:          Roger Evans, Apr 18 1988 (see revisions)
 > Documentation:   REF SIGNALS
 > Related Files:   signals.p sigtab.p signals.ph sig_*.p
 */

    ;;; access to number of signals

#_INCLUDE 'declare.ph';
#_INCLUDE 'signals.ph';

;;; -------------------------------------------------------------------

section $-Sys => sys_max_signal;           

define active sys_max_signal;
    Sys_max_signal;
enddefine;

define updaterof active sys_max_signal(n);
    lvars n, l = datalength(Sys_signal_procs);
    Check_integer(n,1);
    if n fi_< NUM_SYS_SIGS then
        mishap(n,1, 'VALUE FOR sys_max_signal TOO SMALL');
    endif;
    if n fi_> l then
        n fi_- l -> l;
        consvector( destvector(Sys_signal_procs) ->,
                    fast_repeat l times Sig_default_proc endrepeat,
                    n) -> Sys_signal_procs;

        consvector( destvector(Sys_signal_flags) ->,
                    fast_repeat l times Sig_default_flag endrepeat,
                    n) -> Sys_signal_flags;
    endif;
    n -> Sys_max_signal;
enddefine;

endsection; ;;; $-Sys



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 24 1989
        Removed #_IF for S*IGNALS
--- Roger Evans, Sep 26 1988
        Changed SIG_DEFAULT_PROC/FLAG to Sig_default_proc/flag to cope
        with VMS symbol names problem
--- Roger Evans, Apr 18 1988 wrapped with #_IF DEF SIGNALS
 */
