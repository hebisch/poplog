/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvt220screen.p
 > Purpose:         Configure VED for VT220 type terminals
 > Author:          John Williams, Oct 13 1989 (see revisions)
 > Documentation:   HELP * VT220KEYS
 > Related Files:
 */
compile_mode :pop11 +strict;

uses vedvt100screen;

section;

define vedvt220screen();
    vedvt100screen();
    "vt220" -> vedterminalname;

    ;;; vt220 has facilities lacking in old VT100
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

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Oct  4 1990
    Added stuff to speed up screen insertion and deletion`

--- Rob Duncan, Oct 24 1989
        Moved out of "vedvt220.p"
 */
