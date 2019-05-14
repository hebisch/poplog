/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:           C.unix/lib/auto/imcsh.p
 > Purpose:        call ved_imcsh from POP
 > Author:         Roger Evans, Aug 1983 (see revisions)
 > Documentation:  HELP * IMCSH
 > Related Files:
 */

section;

define global vars syntax imcsh = popvedcommand(%"ved_imcsh"%) enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  1 1990
        Now uses popvedcommand
 */
