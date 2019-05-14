/* --- Copyright University of Sussex 1990.  All rights reserved. ---------
 > File:           C.all/lib/auto/ploghelp.p
 > Purpose:        to access Prolog help files from outside VED
 > Author:         Kathryn Seifert, Aug 29 1986 (see revisions)
 > Documentation:  HELP * PLOGTEACH
 > Related Files:  LIB VED_* PLOGHELP, HELP * PLOGTEACH, LIB * VED_PLOGTEACH
 */

section;

define global vars syntax ploghelp = popvedcommand(%"ved_ploghelp"%) enddefine;

endsection;
