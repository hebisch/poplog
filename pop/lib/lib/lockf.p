/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.unix/lib/lib/lockf.p
 > Purpose:         Interface to C lockf
 > Author:          John Williams, Feb  1 1989 (see revisions)
 > Documentation:   HELP * LOCKF, MAN * LOCKF
 > Related Files:
 */

section;

include unix_errno.ph;

exload 'lockf'
    (language C)
    lconstant
        c_lockf(fildes, function, size) : int <- lockf,
        errno                           : int,
        _pop_sigmask(block);
endexload;


constant macro (
    F_ULOCK     =   0,      ;;; Unlock a previously locked section
    F_LOCK      =   1,      ;;; Lock a section for exclusive use
    F_TLOCK     =   2,      ;;; Test and lock a section (non-blocking)
    F_TEST      =   3,      ;;; Test section for other process' locks
    );


define global syslockf(device, lock_op, size);
    dlocal 0 % exacc _pop_sigmask(1), exacc _pop_sigmask(0) %;
    if exacc c_lockf(device_os_channel(device), lock_op, size or 0) == 0 then
        true
    elseif exacc errno == EAGAIN then
        false
    else
        mishap(device, lock_op, 2,
               'Error in lockf (errno = ' sys_>< exacc errno sys_>< ')')
    endif
enddefine;


define global syslock(device, wait);
    if wait then
        until syslockf(device, F_TLOCK, false) do
            syssleep(100)
        enduntil;
        true
    else
        syslockf(device, F_TLOCK, false)
    endif
enddefine;


define global sysunlock(device);
    syslockf(device, F_TEST, 0) and syslockf(device, F_ULOCK, false)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Sep  6 1995
        Fixed for Xved.
--- John Williams, Aug 23 1995
        Rewritten using exload & exacc.
 */
