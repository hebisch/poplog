/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/include/shadowkey.ph
 > Purpose:         Vector subscripts for shadowkey class procedures
 > Author:          John Gibson, May  5 1993
 */

#_TERMIN_IF DEF SHADOWKEY_INCLUDED

section;

;;; class procedures are stored in a vector in the key - these constant
;;; specify location of each class proc in the vector
iconstant macro(
    SHK_CONS            = 1,
    SHK_NC_CONS         = 2,
    SHK_DEST            = 3,
    SHK_NC_DEST         = 4,
    SHK_SUBSCR          = 5,
    SHK_NC_SUBSCR       = 6,
    SHK_FILL            = 7,
    SHK_NC_FILL         = 8,
    SHK_RECOG           = 9,
    SHK_INIT            = 10,
    SHK_REFRESH         = 11,
    SHK_RF_SUBSCR       = 12,
    SHK_NC_RF_SUBSCR    = 13,
    SHK_CHECK           = 14,
    SHK_IMPORT          = 15,

    SHK_NUM_CLASS_PROCS = 15,
);

iconstant SHADOWKEY_INCLUDED = true;

endsection;
