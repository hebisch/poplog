/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/ved/term/vedtvi925screen.p
 >  Purpose:        FOR TELEVIDEO VDU MODEL TVI 925 (Non emulating)
 >  Author:         Jim Hunter, April 1983 (see revisions)
 >  Documentation:  HELP * VED, VEDKEYS
 >  Related Files:  LIB *TVI *TVI925 *TVI925KEYS
 */
compile_mode :pop11 +strict;

section;

define vedtvi925screen();

    "tvi925" -> vedterminalname;
    false -> vedterminalselect;

    true -> vedscreenwrap;

    false ->> vednocharinsert
          ->> vednochardelete
          ->> vednolineinsert
          ->  vednolinedelete;
    true  ->  vednokeypad;

    vedvt52screenxy -> vedscreenxy;

    vedset screen
        charup          = ctrl K
        charleft        = ctrl H
        charright       = ctrl L
        insertchar      = esc Q
        deletechar      = esc W
        insertline      = esc E
        deleteline      = esc R
        cleartail       = esc T
        point           = esc =
        charnormal      = esc G 0
        charbold        = esc G 4
        charunderline   = esc G 8
    endvedset

enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 14 1992
        Removed vedset g*raphic
--- Robert John Duncan, Jan  7 1992
        Added character display attributes
--- Jason Handby, Jul 19 1989
        Extracted from old "tvi925.p" and adapted to use vedset notation.
        Removed redefinition of -vedscreencharright-
*/
