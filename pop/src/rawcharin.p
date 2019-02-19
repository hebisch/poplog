/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/rawcharin.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *CHARIO
 > Related Files:   C.all/src/rawcharout.p
 */

;;; ---------------- STANDARD RAW CHARACTER INPUT --------------------------

#_INCLUDE 'declare.ph'

constant
        procedure Sys$-Io$-Get_char;
    ;

uses (poprawdevin);

;;; ------------------------------------------------------------------------

section $-Sys => rawcharin;

    ;;;; input a character via raw_dev_in
define vars rawcharin();
    dlocal weakref poplinenum;
    if isdevice(raw_dev_in) then
        Io$-Get_char(raw_dev_in)
    else
        ;;; produce mishap
        Check_device(raw_dev_in, true)
    endif
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 16 1988
        Previously in ioraw.p
 */
