/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/src/popdevin.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSIO
 */

;;; ---------------- STANDARD INPUT DEVICE ------------------------------

#_INCLUDE 'declare.ph'

constant
        procedure Sys$-Io$-New_std_dev
    ;

;;; -------------------------------------------------------------------

section $-Sys => popdevin, pop_charin_device;

vars
    dev_in      = false,
    charin_dev  = false,
    ;

define active popdevin;
    dev_in
enddefine;
;;;
define updaterof active popdevin with_nargs 1;
    Io$-New_std_dev((), ident dev_in, 2:11)
enddefine;

define active pop_charin_device;
    charin_dev
enddefine;
;;;
define updaterof active pop_charin_device(dev);
    lvars dev;
    Check_device(dev, 2:10);
    dev -> charin_dev
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 26 1990
        Moved -pop_charin_device- in from charin.p
--- John Gibson, Apr 15 1988
        Moved out of sysio.p
 */
