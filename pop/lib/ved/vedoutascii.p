/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/ved/vedoutascii.p
 > Purpose:         Old -vedoutascii-
 > Author:          John Gibson, Apr  9 1991 (see revisions)
 */
compile_mode :pop11 +strict;

section;

define global vedoutascii() with_nargs 1;
    chain(vedscr_char_out)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jun 10 1991
        Changed name of output procedure to -vedscr_char_out-
 */
