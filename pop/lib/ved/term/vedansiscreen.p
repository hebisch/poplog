/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedansiscreen.p
 > Purpose:         VED screen handling for basic ANSI-compatible terminal
 > Author:          Rob Duncan, Nov  2 1989 (see revisions)
 > Documentation:   REF * VEDTERMINAL, HELP * TERMINAL
 > Related Files:   LIB * VEDANSI, * VEDANSIKEYS
 */
compile_mode :pop11 +strict;

section;

define vedansiscreen();
    "ansi" -> vedterminalname;
    false -> vedterminalselect;
    true -> vedscreenwrap;
    vedansiscreenxy -> vedscreenxy;
    vedset screen;
        clear       = esc [ ; H esc [ 2 J
        cleartail   = esc [ K
        charright   = esc [ C
        charup      = esc [ A
    endvedset;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Oct 11 1990
    Put in cross references
 */
