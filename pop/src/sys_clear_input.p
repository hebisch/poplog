/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/sys_clear_input.p
 > Purpose:         Clear input waiting on a device
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSIO
 */

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'
#_IF DEF UNIX
#_INCLUDE 'unixdefs.ph'
#_ENDIF

constant
        procedure (sys_input_waiting),
        Sys$-no_device_encoding
    ;


;;; ---------------------------------------------------------------------

section $-Sys$-Io => sys_clear_input;

    /*  General clear input procedure
    */
define Clear_input(dev);
    lvars dev, buf = dev!D_IN_BUFFER, res;
    _0 -> buf!BUF_ENCODE_STATE;
    repeat
        _0 ->> buf!BUF_COUNT -> buf!BUF_POSITION;
        quitunless((sys_input_waiting(dev) ->> res) and res /== 0);
        ;;; just read one char and then re-clear the buffer
        fast_apply(dev, _pint(##BUF_START _add _1), buf, 1, dev!D_READ) ->
    endrepeat
enddefine;

define sys_clear_input(dev);
    lvars dev, save_enc_id;

    dlocal 0 %  if dlocal_context == 1 then false -> save_enc_id endif,
                if dlocal_context fi_<= 2 and save_enc_id then
                    save_enc_id -> dev!D_ENCODING_ID
                endif
             %;

    Check_device(dev, 2:1000);      ;;; flush if open for writing

    dev!D_ENCODING_ID -> save_enc_id;
    ident no_device_encoding -> dev!D_ENCODING_ID;
    fast_apply(dev, dev!D_CLEAR_INPUT);

#_IF DEF UNIX
    unless dev!D_FLAGS _bitst _M_D_USER_DEV then
        _extern _pop_set_async_check(_1, dev!D_FILE_DESC, _:O_RDONLY) ->
    endunless
#_ENDIF
enddefine;

endsection;     /* $-Sys$-Io */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 20 1997
        Improved Clear_input so that it (a) quits if sys_input_waiting
        returns 0, and (b) only reads one char using the D_READ procedure
        (which ensures it never tries to read more than is available).
--- John Gibson, Oct 14 1997
        Fixed bug in sys_clear_input: was assigning no_device_encoding to
        dev!D_ENCODING_ID, instead of ident no_device_encoding.
--- John Gibson, Apr  4 1997
        Replaced D_ENCODING with D_ENCODING_ID
--- John Gibson, Feb 24 1997
        Fixed sys_clear_input to locally set dev!D_ENCODING false
        while calling the D_CLEAR_INPUT procedure
--- John Gibson, May 19 1994
        Added call of _extern _pop_set_async_check for Unix
--- John Gibson, Oct 24 1990
        Now a C.all file; uses D_CLEAR_INPUT procedure.
--- John Gibson, Oct 22 1990
        Changed format of dev read procedure args
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Oct 13 1989
        Ved procedure into section Sys$-Ved$-Im
--- John Gibson, Aug 14 1989
        Moved code to clear terminal input to -Clear_term_input- in devio.p
--- John Gibson, Feb 19 1989
        Included io.ph
--- John Williams, Oct  6 1988
        Fixed for VED devices
--- John Gibson, Apr 13 1988
        Moved out of sysio.p
 */
