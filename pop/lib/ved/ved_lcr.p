/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_lcr.p
 >  Purpose:        Converts marked range to lower case
 >  Author:         Aaron Sloman, July 1983 (see revisions)
 >  Documentation:  HELP * VEDCOMMS/ved_lcr
 >  Related Files:  LIB * VED_UCR
 */

section;

define global ved_lcr =
    vedconvertrange(% isuppercode, uppertolower %)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 15 1992
        Changed to use -vedconvertrange-
 */
