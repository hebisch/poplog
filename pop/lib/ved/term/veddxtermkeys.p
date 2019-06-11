/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/term/veddxtermkeys.p
 > Purpose:         VED key bindings for DECwindows DXterm terminal emulator
 > Author:          Rob Duncan, Apr 23 1990 (see revisions)
 */
compile_mode :pop11 +strict;

section;

define veddxtermkeys();
    vedvt220keys();
    vedset keys;
        ;;; F11 is ESC key, so move MARKLO to ESC F12
        marklo      = esc esc [ 2 4 ~
        ;;; PF1 PF1 should do XREFRESH to check for window resize
        "xrefresh"  = esc O P esc O P
    endvedset;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Oct 19 1990
        Added binding for xrefresh
 */
