/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/device_util.p
 > Purpose:         Accessing Device Fields
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSIO
 */


#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'

#_IF DEF VMS
#_INCLUDE 'vmsdefs.ph'
#_ELSEIF DEF WIN32
#_INCLUDE 'win32defs.ph'
#_ENDIF

section $-Sys;

constant
        procedure (Extern$-Get_encoding_funcs),
        no_device_encoding
    ;

vars
        default_device_encoding
    ;

endsection;


;;; --------------------------------------------------------------------

section $-Sys => device_open_name, device_full_name, device_os_channel,
                 device_encoding, device_init_buffer;

define device_open_name(dev);
    lvars dev;
    Check_device(dev, false);
    dev!D_OPEN_NAME
enddefine;

define device_full_name(dev);
    lvars dev;
    Check_device(dev, false);
    dev!D_FULL_NAME
enddefine;
;;;
define updaterof device_full_name(item, dev);
    lvars item, dev;
    Check_device(dev, false);
    unless dev!D_FLAGS _bitst _M_D_USER_DEV then
        mishap(dev, 1, 'USER DEVICE NEEDED')
    endunless;
    item -> dev!D_FULL_NAME
enddefine;


define device_os_channel(dev);
    lvars dev;
    Check_device(dev, true);
    if dev!D_FLAGS _bitst _M_D_USER_DEV then
        false
    else
#_IF DEF WIN32
        Uint_->_pint(dev!D_CTRL_BLK!DCB_HANDLE)
#_ELSEIF DEF VMS
        _pint(dev!D_CTRL_BLK!ICB_CHANNEL)
#_ELSE
        _pint(dev!D_FILE_DESC)
#_ENDIF
    endif
enddefine;
;;;
define updaterof device_os_channel(chan, dev);
    lvars chan, dev;
    Check_device(dev, 2:001);       ;;; not user
#_IF DEF WIN32
    Pint_->_uint(chan, _-1) -> dev!D_CTRL_BLK!DCB_HANDLE
#_ELSEIF DEF VMS
    Check_integer(chan, 0);
    _int(chan) -> dev!D_CTRL_BLK!ICB_CHANNEL
#_ELSE
    Check_integer(chan, 0);
    _int(chan) -> dev!D_FILE_DESC
#_ENDIF
enddefine;


;;; --- DEVICE ENCODING ---------------------------------------------------

define device_encoding(dev);
    lvars dev, encoding, id, want_default = false;
    if isboolean(dev) then (), dev -> (dev, want_default) endif;
    Check_device(dev, false);
    dev!D_ENCODING_ID -> id;
    if id == ident default_device_encoding and not(want_default) then
        "default"
    else
        (fast_idval(id) ->> encoding) and encoding!XP_PROPS
    endif
enddefine;
;;;
define updaterof device_encoding(encoding_name, dev);
    lvars encoding_name, dev, id, tmp;
    if isclosure(dev) and datalength(dev) fi_> 0
    and isdevice(fast_frozval(1,dev) ->> tmp) then
        tmp -> dev
    else
        Check_device(dev, false)
    endif;
    dev!D_ENCODING_ID -> id;
    if id == ident no_device_encoding then
        if encoding_name then
            mishap(dev, 1, 'INVALID DEVICE FOR CHARACTER ENCODING')
        endif
    elseif encoding_name == "default" then
        ident default_device_encoding -> dev!D_ENCODING_ID
    else
        if id == ident default_device_encoding then
            ;;; use own id
            dev!D_ENCODING_OWN_ID ->> id -> dev!D_ENCODING_ID
        endif;
        encoding_name and Extern$-Get_encoding_funcs(encoding_name)
                                -> fast_idval(id)
    endif
enddefine;

define device_init_buffer(dev, len);
    lvars dev, len;
    if device_encoding(dev, true) then
        inits16(len)
    else
        inits(len)
    endif
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 12 1997
        Added device_init_buffer.
--- John Gibson, Apr  8 1997
        Gave device_encoding base procedure an optional boolean arg
        to say want the actual encoding value (i.e. don't return
        "default").
--- John Gibson, Apr  4 1997
        Rewrote device_encoding.
--- Robert Duncan, Mar 18 1997
        Modified device_os_channel for Win32
--- John Gibson, Feb 27 1997
        Added device_encoding
--- John Gibson, Jun 14 1994
        Added updater for device_os_channel
--- John Gibson, Jun 24 1991
        Added updater for -device_full_name-
--- John Gibson, Oct 26 1990
        Made -device_os_channel- return false for user device
--- John Gibson, Feb 19 1989
        Included io.ph
--- John Williams, Oct  6 1988
        Fixed -device_os_channel- for Ved devices
--- John Gibson, Apr 15 1988
        Moved out of sysio.p
 */
