/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/rawcharout.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *CHARIO
 > Related Files:   C.all/src/rawcharin.p
 */

;;; ---------------- STANDARD RAW CHARACTER OUTPUT --------------------------

#_INCLUDE 'declare.ph'

constant
        procedure Sys$-Io$-Put_char;
    ;

vars
        Sys$-Io$- _default_output16_char
    ;

uses (poprawdevout);


;;; ------------------------------------------------------------------------

section $-Sys$-Io => rawcharout;

    ;;;; output a character via raw_dev_out
define vars rawcharout() with_nargs 1;
    lvars dev = raw_dev_out;
    if isdevice(dev) then
        ;;; set this for compatibility with XVed
        dlocal _default_output16_char = `?`;
        Put_char((), dev)
    else
        -> ;    ;;; erase char
        ;;; produce mishap
        Check_device(raw_dev_out, true)
    endif
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 29 1997
        Made it set _default_output16_char.
--- John Gibson, Mar 16 1988
        Previously in ioraw.p
 */
