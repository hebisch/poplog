/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/ved/src/vdcomparg.p
 > Purpose:
 > Author:          John Gibson, Dec 21 1992 (see revisions)
 > Documentation:   REF *VEDPROCS
 */

;;; ------------------- COMPILE VEDARGUMENT -------------------------------

#_INCLUDE 'vddeclare.ph'

;;; -----------------------------------------------------------------------

section $-Sys$-Ved => vedcompilevedargument;

define vars vedcompilevedargument();
    ;;; execute the command on the line
    lvars clos = stringin(vedargument);
    2 -> fast_back(fast_frozval(1, clos));  ;;; start at 2nd char
    pop11_compile(clos);
    sysflush(pop_charout_device);
    sysflush(pop_charerr_device);
enddefine;

endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar  9 1994
        Added flushes for devs
 */
