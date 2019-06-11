/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedansikeys.p
 > Purpose:         VED key bindings for basic ANSI compatible terminal
 > Author:          Aaron Sloman, Oct 1990 (see revisions)
 > Documentation:   HELP * ANSIKEYS
 > Related Files:   LIB * VEDANSISCREEN, LIB * VEDANSI
 */
compile_mode :pop11 +strict;

section;

define vedansikeys();
    vednewkeys();
    'ansi' -> vedkeymapname;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Nov 16 1990
        Sets -vedkeymapname-
 */
