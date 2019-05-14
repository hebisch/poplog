/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/lib/mkssimage.p
 > Purpose:         Create "standard" subsystem saved image
 > Author:          John Williams, Jul 17 1990 (see revisions)
 > Documentation:   HELP * MKSSIMAGE
 > Related Files:   LIB * SUBSYSTEM
 */

#_TERMIN_IF DEF POPC_COMPILING

compile('$popliblib/mkimage.p');

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 21 1992
        Added #_TERMIN_IF for popc
--- Robert John Duncan, May 20 1991
        Now just a synonym for LIB MKIMAGE
--- Robert John Duncan, Oct 16 1990
        Checks that MAIN is a string before assigning to -subsystem-
--- John Williams, Oct 15 1990
        Now recognises -main flag
--- John Williams, Oct  4 1990
        Now uses -syssetup-
--- Simon Nichols, Sep  3 1990
        Changed -Process_args- to deal with nested sublists in -poparglist-.
 */
