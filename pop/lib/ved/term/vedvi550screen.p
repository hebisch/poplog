/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/term/vedvi550screen.p
 >  Purpose:        lib v500 actually for Visual 550.- liable to change
 >  Author:         Aaron Sloman, Nov 1983 (see revisions)
 >  Documentation:  HELP * V550
 >  Related Files:
 */
compile_mode :pop11 +strict;

uses vedvt100screen;

section;

vars vducode = `\^X`; ;;; sets the vi550 back into ordinary vdu mode.

define lconstant resetvdu();
    ;;; convert from graphics or alphagraphics mode to ordinary mode
    rawcharout(vducode);
    rawoutflush();
enddefine;

define vars ved_bright;
    appdata('\^[[1m',rawcharout);   ;;; set high intensity
enddefine;

define vars ved_dim;
    appdata('\^[[0m',rawcharout);   ;;; set low intensity
enddefine;

define vedvi550screen();

    vedvt100screen();
    "vi550" -> vedterminalname;

    vednographics();

    33 -> vedscreenlength;
    false ->> vednocharinsert -> vednochardelete;
    true -> vednokeypad;

    vedset screen
        insertmode          = esc [ 4 h
        deletechar          = esc [ 1 P
        overmode            = esc [ 4 l
    endvedset

enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Jason Handby, Sep 14 1989
        Moved out of old "v550.p" and adapted to use vedset notation.
 */
