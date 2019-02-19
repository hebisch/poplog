/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.unix/src/sys_input_waiting.p
 > Purpose:         Test for input waiting on a device
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSIO
 */

;;; ----------- TEST FOR INPUT WAITING ON A DEVICE -----------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'
#_INCLUDE 'unixdefs.ph'
#_INCLUDE 'unix_select.ph'

section $-Sys;

constant
        procedure (Timed_wait_apply, Io$-Set_term_state)
    ;

weak constant
        procedure (Xt$-Xpt_IO_wait)
    ;

endsection;


;;; ---------------------------------------------------------------------

section $-Sys$-Io => sys_device_wait, sys_input_waiting;

#_IF DEF BERKELEY or DEFV SYSTEM_V >= 4.0

define lconstant Do_dev(dev, chkarg, _fdsetp, _single);
    lvars dev, chkarg, buff, _fdsetp, _single, _fd, _nfds, _minfd;
    Check_device(dev, chkarg);
    if dev!D_FLAGS _bitst _M_D_USER_DEV then
        if chkarg == 2:010 and not(fast_apply(dev, dev!D_TEST_INPUT)) then
            false -> dev
        endif
    elseif dev!D_FLAGS _bitst _M_D_INTERACTIVE then
        ;;; interactive
        if chkarg /== 2:010
        or (dev!D_IN_BUFFER -> buff,
            _zero(buff!BUF_COUNT _sub buff!BUF_POSITION))
        then
            ;;; set mask bit for device
            _fdsetp!FDS_NFDS -> _nfds;
            _fdsetp!FDS_MINFD -> _minfd;
            dev!D_FILE_DESC -> _fd;
            if _fd _sgreq _nfds then
                if _zero(_nfds) then
                    FD_ZERO(_fdsetp@FDS_FDSET);
                    _:FD_SETSIZE -> _minfd
                endif;
                _fd _add _1 -> _fdsetp!FDS_NFDS
            endif;
            if _fd _slt _minfd then _fd -> _fdsetp!FDS_MINFD endif;
            FD_SET(_fd, _fdsetp@FDS_FDSET);
            false -> dev
        endif
    endif;
    if dev then dev elseif _single then [] endif
enddefine;

define lconstant Dev_wait(rd_devs, wr_devs, ex_devs, wait)
                                    -> (rd_ready, wr_ready, ex_ready);
    lvars   dev, rd_devs, wr_devs, ex_devs, wait,
            rd_ready, wr_ready, ex_ready, _nfds;
    lstackmem wait_fdesc_sets _fdsets, struct TIMEVAL _tvp0;
    lconstant errms = 'DEVICE OR DEVICE LIST NEEDED';


    define :inline lconstant SET_UP(_fdsetp, devs, chkarg, list);
        _0 -> _fdsetp!FDS_NFDS;
        if devs /== [] then
            if isdevice(devs) then
                Do_dev(devs, chkarg, _fdsetp, true)
            elseif ispair(devs) then
                [%  for dev in devs do
                        Do_dev(dev, chkarg, _fdsetp, false);
                        _CHECKUSER
                    endfor
                %]
            else
                mishap(devs, 1, errms)
            endif
        else
            []
        endif -> list
    enddefine;

    SET_UP(_fdsets@WFDS_RD_SET, rd_devs, 2:010, rd_ready);
    SET_UP(_fdsets@WFDS_WR_SET, wr_devs, 2:100, wr_ready);
    SET_UP(_fdsets@WFDS_EX_SET, ex_devs, 2:010, ex_ready);

    unless wait and rd_ready == [] and wr_ready == [] and ex_ready == [] then
        false -> wait;
        _0 ->> _tvp0!TIM_SEC -> _tvp0!TIM_USEC
    endunless;

    repeat
        if wait and testdef $-Sys$-Xt$-Xpt_IO_wait
        and (weakref $-Sys$-Xt$-Xpt_IO_wait(_fdsets) ->> _nfds) then
            quitunless((_int(_nfds) ->> _nfds) _gr _2147483647)
        else
            lvars _n, _rdarg, _wrarg, _exarg;
            if _nonzero(_fdsets@WFDS_RD_SET!FDS_NFDS ->> _nfds) then
                _fdsets@WFDS_RD_SET@FDS_FDSET
            else
                _NULL
            endif -> _rdarg;
            if _nonzero(_fdsets@WFDS_WR_SET!FDS_NFDS ->> _n) then
                if _n _sgr _nfds then _n -> _nfds endif;
                _fdsets@WFDS_WR_SET@FDS_FDSET
            else
                _NULL
            endif -> _wrarg;
            if _nonzero(_fdsets@WFDS_EX_SET!FDS_NFDS ->> _n) then
                if _n _sgr _nfds then _n -> _nfds endif;
                _fdsets@WFDS_EX_SET@FDS_FDSET
            else
                _NULL
            endif -> _exarg;

            _extern[SE] select(_nfds, _rdarg, _wrarg, _exarg,
                            if wait then _NULL else _tvp0 endif) -> _nfds;
            quitunless(_neg(_nfds) and _ERRNO == _:EINTR)
        endif;

        _CHECKINTERRUPT;
        if isprocedure(wait) and not(fast_frozval(1,wait)) then
            ;;; timeout expired -- return false for all results
            false ->> rd_ready ->> wr_ready -> ex_ready;
            return
        endif
    endrepeat;


    define lconstant Ready(dev, _fdsetp, _single);
        lvars dev, _fd, _fdsetp, _single;
        if not(dev!D_FLAGS _bitst _M_D_USER_DEV)
        and (dev!D_FILE_DESC -> _fd, FD_ISSET(_fd, _fdsetp@FDS_FDSET)) then
            _nfds _sub _1 -> _nfds;
            dev;
            _CHECKUSER
        elseif _single then
            []
        endif
    enddefine;

    define :inline lconstant ADD_READY(_fdsetp, devs, list);
        if _nonzero(_fdsetp!FDS_NFDS) then
            if isdevice(devs) then
                Ready(devs, _fdsetp, true)
            else
                list nc_<>
                    [%  fast_for dev in devs do
                            quitunless(_nfds _sgr _0);
                            Ready(dev, _fdsetp, false)
                        endfor
                    %]
            endif -> list
        endif
    enddefine;

    ADD_READY(_fdsets@WFDS_RD_SET, rd_devs, rd_ready);
    ADD_READY(_fdsets@WFDS_WR_SET, wr_devs, wr_ready);
    ADD_READY(_fdsets@WFDS_EX_SET, ex_devs, ex_ready);
enddefine;

define sys_device_wait(/*rd_devs, wr_devs, ex_devs,*/ wait)
                                    /* -> (rd_ready, wr_ready, ex_ready) */;
    lvars wait;
    if not(wait) or wait == 0 then
        Dev_wait((), false)
    elseunless isinteger(wait) then
        Dev_wait((), true)
    else
        Timed_wait_apply((), wait, Dev_wait)
    endif
enddefine;

#_ENDIF

define Test_input(dev);
    lvars   dev, buff = dev!D_IN_BUFFER, _fd,
            _count = buff!BUF_COUNT _sub buff!BUF_POSITION;

    returnif(_nonzero(_count)) (_pint(_count));
    returnunless(dev!D_FLAGS _bitst _M_D_INTERACTIVE) (0);

    ;;; test for input on interactive device

    dev!D_FILE_DESC -> _fd;

#_IF DEF BERKELEY or DEFV SYSTEM_V >= 4.0

    lstackmem fd_set _fds, struct TIMEVAL _tvp0, int _ip;

    FD_ZERO(_fds);
    FD_SET(_fd, _fds);
    _0 ->> _tvp0!TIM_SEC -> _tvp0!TIM_USEC;
    if _extern[NI, SE] select(_fd _add _1, _fds, _NULL, _NULL, _tvp0)
       _sgr 0 then
        ;;; return a true result if select says input waiting, but try
        ;;; to make it the number of chars
        if _nonneg(_extern[NI, SE] ioctl(_fd, _FIONREAD, _ip)) then
            _pint(_ip!(int))
        else
            0
        endif
    else
        false
    endif;

#_ELSE
    lvars _fdflags;
    ;;; set no blocking on read, and then try read
    _extern[NI, SE] fcntl(_fd, _F_GETFL) -> _fdflags;
    _extern[NI] fcntl(_fd, _F_SETFL, _fdflags _biset _:O_NDELAY) -> ;
    if dev!D_FLAGS _bitst _M_D_TERMINAL then
        Set_term_state(dev, false)
    endif;
    _extern[NI] read(_fd, buff@BUF_START, buff!BUF_SIZE) -> _count;
    ;;; reset blocking
    _extern[NI] fcntl(_fd, _F_SETFL, _fdflags) -> ;
    if _count _sgr _0 then
        _count -> buff!BUF_COUNT;
        _0 -> buff!BUF_POSITION;
        _pint(_count)
    else
        false
    endif;
#_ENDIF
enddefine;

define sys_input_waiting(dev);
    lvars dev;
    if islist(dev) then
#_IF DEF SCO
        lvars curr_dev;
        for curr_dev in dev do
            Check_device(dev, true);
            fast_apply(dev, dev!D_TEST_INPUT)
        endfor
#_ELSE
        sys_device_wait(dev, [], [], false) -> (,)
#_ENDIF
    else
        Check_device(dev, true);
        fast_apply(dev, dev!D_TEST_INPUT)
    endif
enddefine;

endsection;     /* $-Sys$-Io */



/* --- Revision History ---------------------------------------------------
--- Poplog System, Jan 18 1995 (Julian Clinton)
        Added noddy case for SCO sys_input_waiting.
--- John Gibson, Jun 13 1994
        Added sys_device_wait
--- John Gibson, May 17 1994
        Added [NI] flag on _externs requiring it
--- Robert John Duncan, Jun 22 1992
        SVR4 supports FIONREAD and select(2)
--- Robert John Duncan, Jun 16 1992
        System calls {read,select} replaced by pop_{read,select} from
        "c_core.c"
--- John Gibson, Oct 24 1990
        -sys_input_waiting- rewritten to work through D_TEST_INPUT
        procedure
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- Roger Evans, Jul  2 1990 disabled interrupts while in Poll_mask
--- Roger Evans, Jul  2 1990 minor bug fixes to async code
--- Roger Evans, Jun 28 1990
        Added timeout to Poll_mask
--- Roger Evans, Jun 28 1990 reinstalled as main system version
--- Roger Evans, May 14 1990
        Now uses FD_SET macros and works for MAX_DEVS > 32
        Removed VMS code, pending reinstallation into main system
--- John Gibson, May 11 1990
        Replaced unecessary use of intvec in -Poll_mask- with
        ident _mask (for dlvars _mask).
--- Ian Rogers, Mar 25 1990
        Fixed Simulated-Async code (from public HP version)
--- Roger Evans
        Created xpop version (overriding system one)
        Extended functionality to allow lists and masks
        Merged unix and vms versions
--- John Gibson, Apr 13 1988
        Moved out of sysio.p
 */
