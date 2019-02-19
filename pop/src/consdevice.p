/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/src/consdevice.p
 > Purpose:
 > Author:          John Gibson, Oct 30 1990
 > Documentation:   REF *DEVICE
 */

;;; --------------------- USER DEVICES -----------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'

global constant
        procedure Sys$-Io$-Init_device
    ;


;;; ----------------------------------------------------------------------

section $-Sys => consdevice, device_user_data;

define consdevice(open_name, full_name, user_data, _flags, p_vec) -> dev;
    lvars   dev, open_name, full_name, user_data, p_vec, read_v, write_v,
            seek_p, close_p, _flags;
    lconstant ipvmess = 'consdevice: INVALID PROCEDURE-VECTOR ARGUMENT';

    define lconstant check_arg(arg, n);
        lvars arg, n;
        unless n then
            Check_procedure(arg), arg
        elseif isvector(arg) and datalength(arg) == n then
            appdata(arg, Check_procedure);
            explode(arg)
        else
            mishap(arg, 1, ipvmess)
        endunless
    enddefine;

    Check_string(open_name);
    Check_integer(_flags, 0);
    unless user_data then
        mishap(user_data, 1, 'consdevice: USER DATA ARGUMENT MUST BE NON-FALSE')
    endunless;

    unless isvector(p_vec) and datalength(p_vec) == 4 then
        mishap(p_vec, 1, ipvmess)
    endunless;
    explode(p_vec) -> (read_v, write_v, seek_p, close_p);
    unless read_v or write_v then
        mishap(0, 'consdevice: DEVICE IS NEITHER READABLE NOR WRITEABLE')
    endunless;

    Io$-Init_device() -> dev;
    _M_D_USER_DEV -> dev!D_FLAGS;
    _int(_flags) -> _flags;
    if _flags _bitst _1 then
        ;;; logical terminal
        dev!D_FLAGS _biset _M_D_LOGICAL_TERM -> dev!D_FLAGS
    endif;

    open_name -> dev!D_OPEN_NAME;
    full_name -> dev!D_FULL_NAME;

    if read_v then
        check_arg(read_v, 3)
                    -> (dev!D_READ, dev!D_TEST_INPUT, dev!D_CLEAR_INPUT)
    endif;
    if write_v then
        check_arg(write_v, 2) -> (dev!D_WRITE, dev!D_FLUSH)
    endif;
    if seek_p then check_arg(seek_p, false) -> dev!D_SEEK endif;
    if close_p then check_arg(close_p, false) -> dev!D_CLOSE endif;

    user_data -> dev!D_CTRL_BLK
enddefine;

define device_user_data(dev);
    lvars dev;
    Check_device(dev, false);
    if dev!D_FLAGS _bitst _M_D_USER_DEV then
        dev!D_CTRL_BLK
    else
        false
    endif
enddefine;

endsection;
