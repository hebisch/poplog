/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/poprawdev.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSIO
 */

;;; ------------------ RAW TERMINAL DEVICES ----------------------------

#_INCLUDE 'declare.ph'

;;; -------------------------------------------------------------------

section $-Sys => poprawdevin, poprawdevout;

constant
     raw_dev_in_undef   = struct UNDEF =>> {%"poprawdevin", undef_key%},
     raw_dev_out_undef  = struct UNDEF =>> {%"poprawdevout", undef_key%},
    ;

vars
    raw_dev_in  =  raw_dev_in_undef,
    raw_dev_out =  raw_dev_out_undef,
    ;

define active poprawdevin; raw_dev_in enddefine;
;;;
define updaterof active poprawdevin dev;
    lvars dev;
    unless dev == raw_dev_in_undef then ;;; allows dlocal to reassign saved val
        Check_device(dev, 2:10)     ;;; check readable
    endunless;
    dev -> raw_dev_in
enddefine;

define active poprawdevout; raw_dev_out enddefine;
;;;
define updaterof active poprawdevout dev;
    lvars dev;
    unless dev == raw_dev_out_undef then    ;;; allows dlocal to reassign saved val
        Check_device(dev, 2:100)    ;;; check writeable
    endunless;
    dev -> raw_dev_out
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, May 22 1997
        Made undefs constants instead of lconstants
--- John Gibson, May 16 1996
        Changed both updaters to accept the original undef records as
        legal values (allows a dlocal to reassign the saved value).
--- John Gibson, Nov  6 1990
        Changed calls to -Check_device- to check read/writeability
--- John Gibson, Apr 15 1988
        Moved out of sysio.p
 */
