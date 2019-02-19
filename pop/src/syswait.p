/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/syswait.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF * SYSUTIL
 */

;;; -------------------- WAIT FOR O/S PROCESS -----------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'signals.ph'      ;;; For OSPROC_ stuff

constant
        procedure (syshibernate)
    ;

vars
        Sys$-os_process_list
    ;

;;; ----------------------------------------------------------------------

section $-Sys => sys_wait;

#_IF DEF WIN32

    /*  Win32 Process IDs are inadequate for identifying processes within
        Poplog because they're recycled so quickly. Instead, a process is
        represented by its HANDLE which supports waiting directly, leading
        to a much simpler implementation.
    */
constant procedure ( Check_handles, Handle_wait, $-sys_get_process_status, );

define sys_wait(hnd) -> (hnd, status);
    lvars _count = Check_handles(hnd, "PROCESS", true);
    if _count == 0 then mishap(0, 'NO PROCESSES TO WAIT FOR') endif;
    if Handle_wait(_count, false) ->> hnd then
        sys_get_process_status(hnd) -> status;
    endif;
enddefine;


#_ELSE  ;;; not(DEF WIN32)

define sys_wait(pid) /* -> (pid, status) */;
    lvars pid, Pid, proc, _waited;

    define lconstant find_proc(pid);
        lvars pid, proc;
        fast_for proc in os_process_list do
            returnif(fast_subscrv(OSPROC_PID,proc) = pid) (proc)
        endfor;
        mishap(pid, 1, 'sys_wait: INVALID OR NONEXISTENT PROCESS ID')
    enddefine;

    define lconstant is_dead(proc);
        lvars proc, pair, lpair = false;
        returnif(fast_subscrv(OSPROC_FLAGS,proc) &&=_0 OSPROCF_DEAD) (false);
        fast_for pair on os_process_list do
            if fast_front(pair) == proc then
                fast_back(pair) -> if lpair then fast_back(lpair)
                                   else os_process_list
                                   endif;
                return(fast_subscrv(OSPROC_PID,proc),
                        fast_subscrv(OSPROC_STATUS,proc), true)
            else
                pair -> lpair
            endif
        endfor
    enddefine;

    define lconstant abexit(pid);
        lvars pid;

        define lconstant set_no_wait(pid);
            lvars pid, proc;
            fast_for proc in os_process_list do
                nextunless(fast_subscrv(OSPROC_PID,proc) = pid);
                if is_dead(proc) then
                    () -> (,)
                else
                    fast_subscrv(OSPROC_FLAGS,proc) fi_&&~~ OSPROCF_WILL_WAIT
                                -> fast_subscrv(OSPROC_FLAGS,proc);
                endif;
                return
            endfor
        enddefine;

        if isintegral(pid) then
            set_no_wait(pid)
        elseif pid then
            applist(pid, set_no_wait)
        endif
    enddefine;

    dlocal 0 % if dlocal_context == 1 then false -> _waited endif,
               if dlocal_context == 2 and _waited then abexit(pid) endif
             %;

    repeat
        if pid then
            if isintegral(pid) then
                returnif(is_dead(find_proc(pid))) ()
            else
                listlength(pid) -> ;        ;;; check a list
                if pid == [] then mishap(pid, 1, 'NON-EMPTY LIST NEEDED') endif;
                fast_for Pid in pid do
                    returnif(is_dead(find_proc(Pid))) ()
                endfor
            endif

        else
            ;;; wait for any process
            if os_process_list == [] then
                mishap(0, 'sys_wait: NO CHILD PROCESSES TO WAIT FOR')
            endif;
            fast_for proc in os_process_list do
                returnif(is_dead(proc)) ()
            endfor
        endif;

        true -> _waited;
        syshibernate()
    endrepeat
enddefine;

#_ENDIF

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, May 24 1996
        Created separate definition for Win32 which uses process handles
        rather than PIDs
--- John Gibson, May 27 1994
        Changed to unset OSPROCF_WILL_WAIT on abnormal exit
--- John Gibson, Apr 19 1994
        Totally rewritten as new sys_wait (common to both Unix and VMS)
--- John Gibson, Dec  8 1992
        Rewritten to use waitpid + WNOHANG instead of wait, so that
        the actual waiting can be done with syshibernate
--- John Gibson, Jan 15 1991
        Changed to use _extern wait_popintr
--- John Gibson, Jan  3 1991
        Replaced W*AIT_EI with wait
--- John Gibson, May 15 1990
        Changed behaviour of interrupted sys*wait -- now only returns
        false for Ctrl-C interrupt, otherwise services interrupt
        and tries again.
--- John Gibson, Aug 22 1989
        Repalced #_IF with use of W*AIT_EI defined in unixdefs.ph
--- John Gibson, Mar 17 1988
        Moved out of sysutil.p
 */
