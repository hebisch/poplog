/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_mark.p
 > Purpose:         Command to mark a named range
 > Author:          John Gibson, Mar 13 1992
 > Documentation:   REF *VEDCOMMS
 */
compile_mode :pop11 +strict;

section;

define ved_mark;
    ved_mark_named_range(consword(vedargument));
enddefine;

endsection;
