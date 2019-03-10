/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/src/os_process.p
 > Purpose:         O/S process management
 > Author:          Robert John Duncan, Sep  5 1994
 > Documentation:   REF * SYSUTIL
 > Related Files:   C.unix/src/sysfork.p
                    C.vms/src/sys_spawn.p
                    C.win32/src/sys_create_process.p
 */

#_INCLUDE 'declare.ph'
#_INCLUDE 'signals.ph'

constant procedure ($-Sys$-Waitpid);

;;; -----------------------------------------------------------------------

section $-Sys => pop_status;

vars
    pop_status      = 0,
    os_process_list = [],
    ;

    /*  Adds a new process to os_process_list
    */
define Add_os_process_entry(will_do_wait, astp, pid);
    lvars will_do_wait, astp, pid, proc;
    initv(OSPROC_VECLEN) -> proc;
    pid     -> fast_subscrv(OSPROC_PID,proc);
    if will_do_wait then OSPROCF_WILL_WAIT else 0 endif
            -> fast_subscrv(OSPROC_FLAGS,proc);
    astp    -> fast_subscrv(OSPROC_ASTP,proc);
    proc :: os_process_list -> os_process_list
enddefine;

    /*  Called in response to SIG_CHILD
    */
define Get_os_process_trap();
    lvars proc, flags, pid, status, astp, pair, lpair = false, _pid, _res;
    dlvars _status;
    fast_for pair on os_process_list do
        fast_front(pair) ->  proc;
        fast_subscrv(OSPROC_FLAGS,proc) -> flags;
        if flags &&/=_0 OSPROCF_DEAD then
            pair -> lpair;
            nextloop
        endif;
        fast_subscrv(OSPROC_PID,proc) -> pid;
        Pint_->_uint(pid, _-1) -> _pid;

        repeat
            Waitpid(_pid, ident _status) -> _res;

            if _res == termin then
                ;;; terminated
                Uint_->_bigint(_status) ->> status
                                        -> fast_subscrv(OSPROC_STATUS,proc);
                flags fi_|| OSPROCF_DEAD -> fast_subscrv(OSPROC_FLAGS,proc);
                unless flags &&/=_0 OSPROCF_WILL_WAIT then
                    ;;; will not do a sys_wait for this process, so can
                    ;;; remove proc entry now
                    fast_back(pair) -> if lpair then fast_back(lpair)
                                       else os_process_list
                                       endif
                endunless;
                if fast_subscrv(OSPROC_ASTP,proc) ->> astp then
                    if ispair(astp) then fast_front(astp) else astp endif
                            (% pid, status %);
                    if ispair(astp) then conspair((),fast_back(astp)) endif
                        -> astp
                endif;
                return(astp)

            elseif _res then
                ;;; not terminated
                quitloop
            endif
        endrepeat;

        pair -> lpair
    endfor;
    false
enddefine;

endsection;     /* $-Sys */
