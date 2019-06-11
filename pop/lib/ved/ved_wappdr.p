/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_wappdr.p
 > Purpose:         Write and append marked range to file then delete it
 > Author:          Aaron Sloman, Oct 17 1988 (see revisions)
 > Documentation:   HELP * VEDCOMMS
 > Related Files:   LIB * VED_WAPPR
 */
compile_mode :pop11 +strict;

;;; <ENTER> wappdr <file>
;;;     write marked range appended to <file> then delete it

section;

define vars ved_wappdr;
    ved_wappr();
    ved_d();
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 12 1989
        Now uses define <name> = ...
 */
