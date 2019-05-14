/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/popval.p
 > Purpose:         Old way to compile a list
 > Author:          John Gibson, Apr 16 1991 (see revisions)
 > Documentation:   REF * OBSOLETE
 */

compile_mode :pop11 +strict;

section;

define global popval(list);
    lvars list;
    null(list) ->;          /* Check it's a list */
    pop11_compile(list);
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  9 1993
        Now checks that its argument is a list.
 */
