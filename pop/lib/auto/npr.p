/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/auto/npr.p
 > Purpose:         Print an item followed by a newline
 > Author:          John Williams, Jul 10 1985 (see revisions)
 > Documentation:   HELP * NPR
 > Related Files:   LIB * NPRINTF
 */
compile_mode:pop11 +strict;

section;

define global npr() with_nargs 1;
    pr();
    cucharout(`\n`)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, May 15 1990
        Revised header
 */
