/*  --- Copyright University of Sussex 1990. All rights reserved. ----------
 >  File:           C.all/lib/auto/im.p
 >  Purpose:        For running ved_im from POP
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:
 */

section;

define global vars syntax im = popvedcommand(%"ved_im"%) enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 31 1990
        Changed to syntax word, using popvedcommand
 */
