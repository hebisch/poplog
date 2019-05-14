/* --- Copyright University of Sussex 1990.  All rights reserved. ---------
 > File:           C.all/lib/auto/plogshowlib.p
 > Purpose:        Accesses prolog library files from outside editor
 > Author:         POP System, Aug 27 1986 (see revisions)
 > Documentation:  HELP * PLOGSHOWLIB
 > Related Files:  LIB * VED_PLOGSHOWLIB, LIB * PLOGHELP
 */

section;

define global vars syntax plogshowlib = popvedcommand(%"ved_plogshowlib"%) enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 31 1990
        Changed to syntax word, using pdpart of -help-
 */
