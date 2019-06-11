/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_errs.p
 > Purpose:         Toggle value of -vedlmr_errs_in_file-
 > Author:          Unknown (see revisions)
 > Documentation:   REF * VEDCOMMS
 > Related Files:   LIB * TRYSETVALOF
 */
compile_mode :pop11 +strict;

section;

define ved_errs =
    vedtrysetvalof(% "vedlmr_errs_in_file",
                    'ERRORS NOW IN FILE',
                    'ERRORS NOW NOT IN FILE' %)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Nov 13 1990
        Converted to define = syntax.
 */
