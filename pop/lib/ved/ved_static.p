/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_static.p
 > Purpose:         Toggle value of -vedstatic-
 > Author:          Mark Rubinstein, April 1985 (see revisions)
 > Documentation:   REF * VEDCOMMS
 > Related Files:   LIB * TRYSETVALOF
 */
compile_mode :pop11 +strict;

section;

define ved_static
    = vedtrysetvalof(% "vedstatic", 'STATIC ON', 'STATIC OFF' %)
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Williams, Nov 13 1990
        Converted to define = syntax.
 */
