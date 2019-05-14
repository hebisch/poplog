/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:           C.unix/lib/auto/imsh.p
 > Purpose:        call ved_imsh from POP
 > Author:         Roger Evans, Aug 1983 (see revisions)
 > Documentation:  HELP * IMCSH
 > Related Files:
 */

section;

define global vars syntax imsh = popvedcommand(%"ved_imsh"%) enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  1 1990
        Now uses popvedcommand
 */
