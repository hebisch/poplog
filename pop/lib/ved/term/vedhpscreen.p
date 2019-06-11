/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedhpscreen.p
 > Purpose:         VED: Screen setup for standard HP terminals
 > Author:          Rob Duncan, Oct 19 1989 (see revisions)
 > Documentation:
 > Related Files:
 */

compile_mode :pop11 +strict;

section;

define vedhpscreen();

    define lconstant hpscreenxy(col, line);
        lvars   col, line;
        dlocal  cucharout = vedscr_char_out, pop_pr_radix = 10;
        printf(col-1, line-1, '\^[&a%py%pC');
        col -> vedscreencolumn;
        line -> vedscreenline;
    enddefine;

        ;;; special version of insert line which clears the bottom line
        ;;; of the screen before opening the new line to prevent text
        ;;; from being remembered below
    define lconstant hpinsertline();
        dlocal cucharout = vedscr_char_out, pop_pr_radix = 10;
        printf(vedscreenline-1, vedscreenlength-1,
            '\^[&a%pY\^[M\^[&a%pY\^[L');
    enddefine;

    "hp" -> vedterminalname;
    false -> vedterminalselect;

    /* 24 x 80 screen, autowrap */

    80 -> vedscreenwidth;
    24 -> vedscreenlength;
    true -> vedscreenwrap;

    /* enable modes */

    false ->> vednocharinsert
          ->> vednochardelete
          ->> vednolineinsert
          ->> vednolinedelete
          ->  vednokeypad;

    /* set screen control vars */

    hpscreenxy -> vedscreenxy;
    hpinsertline -> vedscreeninsertline;

    vedset screen;

        charup          = esc A
        chardown        = esc B
        charright       = esc C
        insertmode      = esc Q
        overmode        = esc R
        deletechar      = esc P
        deleteline      = esc M
        clear           = esc & a 0 y 0 C esc J
        cleartail       = esc K
        setpad          = esc & s 1 A
        resetpad        = esc & s 0 A

    endvedset;

    /* redrawing is safer than scrolling */

    false -> vedscrollscreen;

enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert Duncan, May 11 1993
        Added a cleverer version of insertline which clears the bottom line
        of the screen to prevent text being remembered below; also disabled
        scrolling which seems to get stuck sometimes.
--- Robert John Duncan, Oct 15 1992
        Removed character display attributes again -- they don't behave
        as VED requires
--- Robert John Duncan, Jan  7 1992
        Added character display attributes
 */
