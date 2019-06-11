/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/vedhelpkey.p
 > Purpose:         Procedure to be assigned to keys labelled "Help"
 > Author:          John Williams, Oct  5 1990 (see revisions)
 > Documentation:   REF * vedhelpkey
 > Related Files:   C.all/lib/ved/ved_hkeys.p
 */
compile_mode :pop11 +strict;

section;

define vars vedhelpkey();
    ved_hkeys()
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 28 1992
        Made a procedure since ved_hkeys is variable.
 */
