/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.unix/src/sys_io_control.p
 > Purpose:         User interface to Unix 'ioctl' system call
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSIO
 */


#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'
#_INCLUDE 'unixdefs.ph'
#_INCLUDE 'unix_tty.ph'

section $-Sys$-Io;

constant
        procedure (Get_tty_params, Set_tty_params, Fix_dev_params)
    ;

endsection;

;;; ----------------------------------------------------------------------

section $-Sys$-Io => sys_io_control;

define sys_io_control(dev, _mode) -> _res;
    lvars dev, devparams = false, argstruct = false, _mode, _res, _argbptr;
;;; _extern printf('sys_io_control\n') -> _;
;;; _extern fflush(_0) -> _;
    unless isdevice(dev) then
        ;;; structure argument supplied
        ((), dev, _mode) -> (dev, _mode, argstruct);
        if isintegral(argstruct) then
            Pint_->_uint(argstruct, _-1) -> _argbptr
        elseif iscompound(argstruct)
        and argstruct!KEY!K_FLAGS _bitst _:M_K_BYTE_ACCESS then
            argstruct@V_BYTES -> _argbptr
        else
            mishap(argstruct, 1, 'INVALID 3RD ARGUMENT FOR SYS_IO_CONTROL');
        endif
    else
        _NULL -> _argbptr
    endunless;

    Check_device(dev, 1);
    Pint_->_uint(_mode, _-1) -> _mode;

    if systrmdev(dev) then
        if Fix_dev_params(dev, false) ->> devparams then
            Set_tty_params(dev, devparams, _TSETPARM_W);
        endif;
    endif;

    (_extern[NI, SE] ioctl(dev!D_FILE_DESC, _mode, _argbptr))
          _sgreq _0 -> _res;

    if devparams and _res then
        Get_tty_params(dev, devparams);
        devparams -> dev!D_UNIT_P!UNT_CURR_TTPARAMS;
        ATT_PARM_CHANGED -> dev!D_UNIT_P!UNT_TTPARM_STATUS;
    endif;
enddefine;

endsection;     /* $-Sys$-Io */



/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jun  1 1992
        Simplified so that it no longer has to recognise lots of different
        ioctl codes for the various terminal interfaces: now it just does
        the ioctl as requested but keeps the device parameters in step.
--- John Gibson, Dec 12 1990
        D_UNIT_P structure now contains underlying tty params
--- John Gibson, Aug 28 1989
        Revised for new procedures dealing with tty params
--- John Gibson, Aug 22 1989
        Changed to allow Bobcat system to use Berkeley job control params.
        (Bobcat now has BERKELEY set but not SYSTEM_V).
--- John Gibson, Mar 23 1989
        System V tty params struct now correctly called TERMIO instead
        of SGTTYB.
--- John Gibson, Feb 19 1989
        Included io.ph
--- John Williams, Oct  6 1988
        Fixed for VED devices
--- John Gibson, Mar 13 1988
        Moved here from sysio.p
 */
