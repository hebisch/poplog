/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/include/xveddefs.ph
 > Purpose:         Defines location of XVED
 > Author:          Jonathan Meyer, Apr  1 1991 (see revisions)
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF XVEDDEFS_INCLUDED

section;

iconstant macro (

    XVED =      '$usepop/pop/x/ved/',

    XVEDSRC     = XVED dir_>< 'src/',
    XVEDLIB     = XVED dir_>< 'lib/',
    XVEDAUTO    = XVED dir_>< 'auto/',

    XVEDHELP    = XVED dir_>< 'help/',
    XVEDREF     = XVED dir_>< 'ref/',
    XVEDTEACH   = XVED dir_>< 'teach/',
    XVEDSYSDOC  = XVED dir_>< 'sysdoc/',

    XVEDXRDB    = XVED dir_>< 'xrdb/',
    XVEDBITMAPS = XVED dir_>< 'bitmaps/',
);

iconstant XVEDDEFS_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 28 1993
        Removed XVEDI*NCLUDE -- now just uses standard X include dir
--- John Gibson, Oct 31 1991
        Uses dir_>< instead of sys_>< etc
--- Jonathan Meyer, Jun 17 1991
        Removed XVED_LOAD_OPTIONS (now in XVEDSRC/xved.p)
--- Jonathan Meyer, May 30 1991
        Removed xvedhome
 */
