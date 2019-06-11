/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_noul.p
 > Purpose:         Remove underlining from a file
 > Author:          John Williams, Apr  7 1989 (see revisions)
 > Documentation:
 */

section;

define global ved_noul
    = veddo(% 'chat f -u' %)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, May 29 1998
        Changed to use ved_chat.
--- John Williams, Apr  7 1989
        Completely re-written
 */
