/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/lib/include/cons_network_device.ph
 > Purpose:         Subscripts for device_user_data vector in device
 >                  produced by cons_network_device.
 > Author:          John Gibson, Dec 23 1997
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF CONS_NETWORK_DEVICE_INCLUDED

section;

iconstant macro (
    NTD_ATTR_LIST   = 1,
    NTD_IODEV       = 2,
    NTD_REM_NBYTES  = 3,
    NTD_BUF         = 4,
    NTD_BUF_NBYTES  = 5,
    NTD_BUF_POS     = 6,
    NTD_NET_ASCII   = 7,
    NTD_VEC_LEN     = 7,
);

iconstant CONS_NETWORK_DEVICE_INCLUDED = true;

endsection;
