/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/src/popdevout.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSIO
 */

;;; ------------- STANDARD OUTPUT & ERROR DEVICES --------------------------

#_INCLUDE 'declare.ph'

constant
        procedure Sys$-Io$-New_std_dev
    ;

;;; -------------------------------------------------------------------

section $-Sys => popdevout, popdeverr, pop_charout_device, pop_charerr_device;

vars
    dev_out = false,
    dev_err = false,

    charout_dev = false,
    charerr_dev = false,
    ;

define active popdevout;
    dev_out
enddefine;
;;;
define updaterof active popdevout with_nargs 1;
    Io$-New_std_dev((), ident dev_out, 2:101)
enddefine;

define active popdeverr;
    dev_err
enddefine;
;;;
define updaterof active popdeverr with_nargs 1;
    Io$-New_std_dev((), ident dev_err, 2:101)
enddefine;

define active pop_charout_device;
    charout_dev
enddefine;

define updaterof active pop_charout_device(dev);
    lvars dev;
    Check_device(dev, 2:100);
    dev -> charout_dev
enddefine;

define active pop_charerr_device;
    charerr_dev
enddefine;

define updaterof active pop_charerr_device(dev);
    lvars dev;
    Check_device(dev, 2:100);
    dev -> charerr_dev
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 26 1990
        Moved in -pop_charout/err_device- from charout.p
--- John Gibson, Apr 15 1988
        Moved out of sysio.p
 */
