/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.unix/src/syspipe.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF * SYSIO
 */
;;; ----------------------- UNIX PIPES -----------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'unixdefs.ph'

constant
        procedure (Sys_fd_open_check)
    ;


;;; ---------------------------------------------------------------------

section $-Sys$-Io => syspipe;

define New_pipe() -> (_fd_out, _fd_in);
    lvars _res, _retry = 1, _fd_out, _fd_in;
    lstackmem int _fdp[2];
    repeat
        _extern[SE] pipe(_fdp) -> _res;
        quitif((Sys_fd_open_check(_pint(_res), false, _retry) ->> _retry)
                        fi_< 0)
    endrepeat;
    if _neg(_res) then Syserr_mishap(0, 'CAN\'T CREATE PIPE') endif;

    _fdp!(int)[_0] -> _fd_in;
    _fdp!(int)[_1] -> _fd_out;

    ;;; set close-on-exec flags
    _extern[NI] fcntl(_fd_in, _F_SETFD, _1) -> ;
    _extern[NI] fcntl(_fd_out, _F_SETFD, _1) ->
enddefine;

define syspipe(arg3);
    lvars arg3, (_fd_out, _fd_in) = New_pipe();
    Sys_cons_device('pipe_out', false, O_WRONLY, arg3, _pint(_fd_out), true),
    Sys_cons_device('pipe_in', false, O_RDONLY, arg3, _pint(_fd_in), true)
enddefine;

endsection;     /* $-Sys$-Io */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun  1 1994
        Changed to use new lstackmem construct
--- John Gibson, May 24 1994
        # Changed syspipe to use Sys_fd_open_check
        # Moved sockets stuff to library
--- Robert John Duncan, Jun  2 1993
        Sockets enabled for SVR4; added a dummy exload to ensure that the
        required library is pulled in
--- Robert John Duncan, Jun 23 1992
        SunOS 5.0 is no longer BSD but still has sockets
--- John Gibson, Jan 16 1991
        Added -New_pipe-
--- John Gibson, Feb 27 1990
        Added extra arg to calls of -Sysgarbage-.
--- John Gibson, Aug 22 1989
        Test for presence of sockets now just BERKELEY (since Bobcat
        has this set).
--- Roger Evans, Jun  6 1988
        Added extra argument to Cons*_device calls
--- John Gibson, Mar 31 1988
        Moved out of sysio.p. Made definition of -syssocket- dependent on
        BERKELEY or BSD_SOCK ETS.
 */
