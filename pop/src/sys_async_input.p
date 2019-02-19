/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.unix/src/sys_async_input.p
 > Purpose:         controlling asynchronous I/O on devices
 > Author:          Roger Evans (see revisions)
 > Documentation:   REF * SYSIO
 */

;;; ----------- CONTROL ASYNCHRONOUS I/O ON A DEVICE --------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'
#_INCLUDE 'signals.ph';
#_INCLUDE 'unixdefs.ph'

global constant
        procedure (sys_raise_ast)
    ;


;;; ---------------------------------------------------------------------

section $-Sys$-Io => sys_async_io;

define lconstant Raise_Sigio = sys_raise_ast(% SIG_IO %) enddefine;

define sys_async_io(dev, ioset) -> trap_p;
    lvars dev, ioset, trap_p = false, _ioset;
    Check_integer_range(ioset, RD_SET, EX_SET);
    Check_device(dev, if ioset == WR_SET then 2:101 else 2:11 endif);
    _int(ioset) -> _ioset;
    if _nonzero(_extern _pop_get_async_fd(dev!D_FILE_DESC, _ioset)) then
        if (dev!D_UNIT_P!UNT_IO_TRAP[_ioset] ->> trap_p) == Raise_Sigio then
            true -> trap_p
        endif
    endif
enddefine;

define updaterof sys_async_io(trap_p, dev, ioset);
    lvars dev, ioset, trap_p, _ioset, _on;
    Check_integer_range(ioset, RD_SET, EX_SET);
    Check_device(dev, if ioset == WR_SET then 2:101 else 2:11 endif);
    if trap_p == true then
        Raise_Sigio -> trap_p
    elseif trap_p then
        Check_astp_arg(trap_p)
    endif;
    if trap_p then _1 else _0 endif -> _on;
    _int(ioset) -> _ioset;
    unless _nonneg(_extern _pop_set_async_fd(_on, dev!D_FILE_DESC, _ioset))
    then
        Syserr_mishap(dev, 1, 'ERROR SETTING/CLEARING ASYNC I/O ON DEVICE')
    endunless;
    trap_p -> dev!D_UNIT_P!UNT_IO_TRAP[_ioset]
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul  8 1994
        Now sys_async_io taking 2nd arg 0,1,2 to mean read,write,except
--- John Gibson, May  5 1994
        Tidied up and changed to use new _extern routines
--- John Gibson, Apr 13 1994
        Changed updater to use Check_astp_arg instead of Check_procedure
--- John Gibson, Jun 29 1992
        Removed Re*set_async_dev
--- John Gibson, Jan 15 1991
        Rewritten to use _extern _pop_set_async_fd.
--- John Gibson, Jan  5 1991
        Changed simulated SIGIO to use -sys_timer- .
--- John Gibson, Dec  3 1990
        Replaced _pop_a*dd_sig with _pop_add_ast taking ast type as 1st arg
--- Roger Evans, Nov 19 1990 added extra indirection to pop_async_mask
--- Roger Evans, Nov  9 1990 added Xpt_read_wait
--- John Gibson, Oct 24 1990
        Replaced -Check_re*addev- with -Check_device-
--- John Gibson, Aug 24 1990
        Reorganised, tidied up
--- John Gibson, Aug 19 1990
        Moved to C.unix/src
--- Roger Evans, Jul 11 1990 fixed another in async code - now works!!
--- Roger Evans, Jul  2 1990 minor bug fixes to async code
--- Roger Evans, Jun 29 1990
        Added full support for simulated async_io (Async_read_wait etc.)
--- Roger Evans, Jun 28 1990 installed in main system
--- Roger Evans, Jun 26 1990 added Clear_async_devs
--- Roger Evans, Jun 25 1990 fixed ITIMERVAL definitions
--- Roger Evans, May  9 1990 fixed minor merging error in Disable_poll
 */
