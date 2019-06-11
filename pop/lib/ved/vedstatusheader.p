/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/vedstatusheader.p
 > Purpose:         Old string containing statusline header
 > Author:          John Gibson, Mar  3 1992
 > Documentation:   REF *OBSOLETE
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

    /*  This is not used by VED any more. It remains for upward
        compatibility. Code that previously used datalength(vedstatusheader)
        should now use vedstatusheaderlen.
    */
global vars vedstatusheader =
    consdstring(
        repeat 3 times vedscreenstatus_-_mark endrepeat,    ;;; '---'
        vedscreenstatus_-|_mark,                            ;;; '-|'
        repeat 4 times `\s` endrepeat,                      ;;; '    ',
        vedscreenstatus_|-_mark,                            ;;; '|-'
        9);

endsection;
