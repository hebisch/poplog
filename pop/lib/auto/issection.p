/* --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.all/lib/auto/issection.p
 > Purpose:        Recogniser for sections
 > Author:         John Williams, Mar 23 1987 (see revisions)
 */
compile_mode:pop11 +strict;

section;

define global issection() with_nargs 1;
    datakey() == section_key
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 10 1992
        Made a procedure
 */
