/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.unix/src/sysio.p
 > Purpose:         Basic system I/O procedures (unix)
 > Author:          John Gibson (see revisions)
 */

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'
#_INCLUDE 'unixdefs.ph'
#_INCLUDE 'unix_tty.ph'

global constant
        procedure (Sys$-Io$-Kill_device, Sys$-Io$-Sync_or_close)
    ;


;;; ----------------------------------------------------------------------

    /*  This only ever becomes true in a system with vfork */
global vars
    Sys$- _vfork_child = false;


section $-Sys$-Io => sysclose, sysflush, sys_link_tty_params;


;;; --- CHANGING STANDARD DEVICES ----------------------------------------

define New_std_dev(new_dev, std_dev_id, check_flags);
    lvars new_dev, std_dev, std_dev_id, check_flags, _stdfd, _tmp;

    fast_idval(std_dev_id) -> std_dev;
    returnif(new_dev == std_dev);
    Check_device(new_dev, check_flags);
    copy(new_dev) -> new_dev;
    std_dev!D_FILE_DESC -> _stdfd;

    if _vfork_child then
        ;;; Berkeley only with vfork
        _extern[NI] pop_close(_stdfd) ->
    else
        ;;; try to keep old standard file open under another descriptor
        if _nonneg(_extern[NI, SE] fcntl(_stdfd, _F_DUPFD, _3) ->> _tmp) then
            _tmp -> std_dev!D_FILE_DESC;
            _extern[NI] fcntl(_tmp, _F_SETFD, _1) -> ;  ;;; set close-on-exec
            _extern[NI] pop_close(_stdfd) ->
        else
            sysclose(std_dev)
        endif;
        new_dev -> fast_idval(std_dev_id)
    endif;

    ;;; dup new device's file descriptor onto _stdfd
    _extern[NI] fcntl(new_dev!D_FILE_DESC, _F_DUPFD, _stdfd) -> ;
    _extern[NI] fcntl(_stdfd, _F_SETFD, _1) -> ;    ;;; set close-on-exec
    _stdfd -> new_dev!D_FILE_DESC
enddefine;


;;; --- SYS- I/O PROCEDURES -----------------------------------------------

define sysclose(dev);
    lvars dev;
    Check_device(dev, false);
    if _vfork_child then
        ;;; Berkeley only with vfork
        _extern[NI] pop_close(dev!D_FILE_DESC) ->
    else
        fast_apply(dev, dev!D_CLOSE);
        Kill_device(dev)
    endif
enddefine;

define sysflush(dev);
    lvars devparams, ctrl_blk, dev, dosync = false;
    if isboolean(dev) then (), dev -> (dev, dosync) endif;
    Check_device(dev, true);
    fast_apply(dev, dev!D_FLUSH);
    if dosync and not(dev!D_FLAGS _bitst _M_D_USER_DEV) then
        Sync_or_close(dev, _extern fsync)
    endif
enddefine;

    /*  Make all terminal devices in dev_list referencing the same
        actual terminal have the same D_CTRL_BLK structure
    */
define sys_link_tty_params(dev_list);
    lvars dev, adev, ctrl_blk, dev_list, unit;
    for dev in dev_list do
        Check_device(dev, 1)
    endfor;
    until dev_list == [] do
        fast_destpair(dev_list) -> dev_list -> dev;
        nextunless(dev!D_FLAGS _bitst _M_D_TERMINAL);
        dev!D_UNIT_N -> unit;
        fast_for adev in dev_list do
            if adev!D_FLAGS _bitst _M_D_TERMINAL and adev!D_UNIT_N == unit then
                ;;; same terminal - give same control block
                dev!D_CTRL_BLK -> adev!D_CTRL_BLK;
                quitloop
            endif
        endfast_for
    enduntil
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 29 1995
        Stopped sysflush calling Sync_or_close on user devices
--- John Gibson, May 17 1994
        Added [NI] flag on _externs requiring it
--- John Gibson, Sep 28 1993
        Made sysflush take an optional 2nd bool arg to mean sync the file
        after flushing.
--- Robert John Duncan, Jun 16 1992
        System call -close- replaced by -pop_close- from "c_core.c"
--- John Gibson, Dec 12 1990
        Underlying device unit now identified by D_UNIT_N/P structures --
        changed -sys_link_tty_params- accordingly.
--- John Gibson, Oct 26 1990
        Added -check_flags- arg to -New_std_dev-
--- John Gibson, Jan 25 1990
        Moved initialisation of _vfork_child from sysfork.p to here
        and removed testdefs on sysvfork.
--- John Gibson, Aug 22 1989
        Included unix_tty.ph
--- John Gibson, May 25 1989
        Made -New_std_dev- retain the old standard device open under
        a different file descriptor.
--- John Gibson, Feb 19 1989
        Included io.ph
--- John Williams, Oct  6 1988
        Fixed for VED devices
--- John Gibson, Mar 31 1988
        Moved -syscreate-, -sysdelete-, -syslink-, -syspipe- etc
        to separate files.
--- John Gibson, Mar 15 1988
        Moved -sysread- and -syswrite- to separate file sysreadwrite.p
--- John Gibson, Mar 13 1988
        -sys_io_control- moved to separate file
--- John Williams, Mar  2 1988
        Added -syssocket- (with help from RogerE and RichB)
--- John Gibson, Feb 11 1988
        Pint_->_uint, Check_integer etc in section Sys
--- John Gibson, Nov 13 1987
        Added -sys_link_tty_params-
--- John Gibson, Nov 12 1987
        Replaced -dev_raw- with -raw_dev_in- and -raw_dev_out-, etc.
        Replaced -sys_purge_terminal- with -sys_clear_input-, which
        now works on any device.
--- John Williams, Jun 15 1987
        locally sets 'umask' 0 when creating back up files
 */
