/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_ucr.p
 >  Purpose:        Converts marked range to upper case
 >  Author:         Aaron Sloman, July 1983 (see revisions)
 >  Documentation:  HELP * VEDCOMMS/ved_ucr
 >  Related Files:  LIB * VED_UCL, *VED_UCW
 */

section;

define global ved_ucr =
    vedconvertrange(% islowercode, lowertoupper %)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 15 1992
        Changed to use -vedconvertrange-
 */
