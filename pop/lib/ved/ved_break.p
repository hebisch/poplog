/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_break.p
 > Purpose:         Toggle value of vedbreak
 > Author:          Mark Rubinstein, Aug 1985 (see revisions)
 > Documentation:   REF * VEDCOMMS
 > Related Files:   LIB * VEDTRYSETVALOF
 */
compile_mode :pop11 +strict;

section;

define ved_break =
    vedtrysetvalof(% "vedbreak", '\{b}line-break on', '\{b}line-break off' %)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Nov 13 1990
        Converted to define = syntax.
 */
