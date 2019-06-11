/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvt100screenextra.p
 > Purpose:         Speed up character insertion and deletion on Vt100
 > Author:          Aaron Sloman, 4 Oct 1990
 > Documentation:   HELP * VT100, REF * VEDTERMINALS, HELP * VEDSET
 > Related Files:   LIB * vedvt220screen
 */
compile_mode :pop11 +strict;

uses vedvt100screen;

section;

define vedvt100screenextra();
    vedvt100screen();

    ;;; Assume screen has facilities lacking in old VT100
    false -> vednocharinsert;
    false -> vednochardelete;

    vedset screen;
        deletechar  = '\^[[P'
        insertmode  = '\^[[h'
        overmode    = '\^[[l'
        insertchar  = '\^[[@'
    endvedset;
enddefine;

endsection;
