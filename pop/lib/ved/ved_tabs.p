/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_tabs.p
 > Purpose:         Toggle value of -vednotabs-
 > Author:          Mark Rubinstein, April 1985 (see revisions)
 > Documentation:   REF * VEDCOMMS
 > Related Files:   LIB * VEDTRYSETVALOF
 */
compile_mode :pop11 +strict;

section;

define ved_tabs
    = vedtrysetvalof(% "vednotabs", '\{b}tabs off', '\{b}tabs on' %)
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Williams, Nov 13 1990
        Converted to define = syntax.
 */
