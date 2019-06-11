/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/ved/term/vedtviscreen.p
 >  Purpose:        For TELEVIDEO MODEL TVI 920C Emulating VT52
 >  Author:         A Sloman, 1982 (see revisions)
 >  Documentation:  HELP * TVI
 >  Related Files:  LIB * TVI925
 */
compile_mode :pop11 +strict;

uses vedvi200screen;

section;

define vedtviscreen();

    vedvi200screen();

    "tvi" -> vedterminalname;
    false -> vedterminalselect;

    false ->> vedscrollscreen
          ->> vednocharinsert
          ->> vednochardelete
          ->> vednolineinsert
          ->  vednolinedelete;
    true  ->  vednokeypad;

    vedset screen
        insertchar      = esc Q
        deletechar      = esc W
        insertline      = esc E
        deleteline      = esc R
        cleartail       = esc T
        point           = esc Y
        charnormal      = esc m esc k
        charbold        = esc j
        charunderline   = esc l
    endvedset

enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jan  7 1992
        Added character display attributes
--- Jason Handby, Sep 14 1989
        Separated out from old "tvi.p" and adapted to use vedset notation.
*/
