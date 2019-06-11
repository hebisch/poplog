/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/helpfor.p
 > Purpose:         POP11 interface to LIB * VED_HELPFOR
 > Author:          Aaron Sloman, June 1982 (see revisions)
 > Documentation:   HELP * HELPFOR
 > Related Files:   LIB * VED_HELPFOR
 */
compile_mode :pop11 +strict;

section;

uses-by_name ved_helpfor;

define vars syntax helpfor
    = popvedcommand(% "ved_helpfor" %)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Nov 13 1990
        Now uses -popvedcommand-
 */
