/* --- Copyright University of Sussex 1990.  All rights reserved. ---------
 > File:           C.all/lib/auto/plogteach.p
 > Purpose:        to access Prolog teach files from outside VED
 > Author:         Kathryn Seifert, Aug 29 1986 (see revisions)
 > Documentation:  HELP * PLOGTEACH
 > Related Files:  LIB * PLOGHELP, HELP * PLOGTEACH, LIB * VED_PLOGTEACH
 */

section;

define global vars syntax plogteach = popvedcommand(%"ved_plogteach"%) enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 31 1990
        Changed to syntax word, using pdpart of -help-
 */
