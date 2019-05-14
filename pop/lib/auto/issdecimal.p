/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:           C.all/lib/auto/issdecimal.p
 > Purpose:        Recogniser for single length decimals
 > Author:         John Williams, Jan 29 1987 (see revisions)
 > Documentation:  HELP * ISSDECIMAL
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define issdecimal(item);
    issimple(item) and not(isinteger(item))
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 17 1995
        Tidied up
 */
