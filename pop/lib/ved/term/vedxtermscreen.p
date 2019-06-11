/* --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:            C.all/lib/ved/term/vedxtermscreen.p
 > Purpose:         Ved screen control for xterm vt102 emulator
 > Author:          Roger Evans et al. (see revisions)
 > Documentation:   HELP * VEDXTERM
 > Related Files:   LIB * VEDXTERM, * VEDXTERMKEYS
 */
compile_mode :pop11 +strict;

/*
 *  This is basically a vt100, but with extras to cope with windows of
 *  different sizes.
 */

section;

define vedxtermsize();
    lvars ch, row = 0, col = 0;
    appdata('\^[7\^[[r\^[[999;999H\^[[6n', rawcharout);
    until rawcharin() == `\^[` do enduntil;
    unless rawcharin() == `[` then return(false) endunless;
    until (rawcharin() ->> ch) == `;` or ch == `R` do
        unless isnumbercode(ch) then return(false) endunless;
        row * 10 + (ch - `0`) -> row;
    enduntil;
    unless ch == `R` then
        until (rawcharin() ->> ch) == `R` do
            unless isnumbercode(ch) then return(false) endunless;
            col * 10 + (ch - `0`) -> col;
        enduntil;
    endunless;
    rawcharout(`\^[`); rawcharout(`8`); rawoutflush();
    col; row;
enddefine;

define vedxrefresh();
    lvars row, col;
    if (vedxtermsize() ->> row) then
        -> col;
        returnunless(vedchecksize(row, col))(vedresize(row, col));
    endif;
    vedrefresh();
enddefine;

define lconstant vedxterminit();
    lvars row, col;
    if (vedxtermsize() ->> row) then
        -> col;
        unless vedchecksize(row, col) then
            vedresize(row, col, false);
        endunless;
    endif;
enddefine;

define vedxtermscreen();
    returnif(vedterminalname == "xterm");
    vedvt100screen();
    "xterm" -> vedterminalname;
    ;;; default window size
    34 -> vedscreenlength;
    80 -> vedscreenwidth;
    true -> vedscreenwrap;
    ;;; reset window size each time ved is entered
    vedxterminit -> vvedscreeninit;
    ;;; extra features over a basic vt100
    vedset screen;
    /*
        insertchar  = esc [ @
        deletechar  = esc [ P
    */
        insertline  = esc [ L
        deleteline  = esc [ M
    endvedset;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 11 1992
        Got rid of variable vedxtermcore and instead made vedxtermscreen
        do nothing if vedterminalname is already "xterm"
--- Robert John Duncan, Oct  3 1990
        Changed -vedxtermrefresh- and -vedxterminit- to use -vedchecksize-
--- Rob Duncan, Jun 21 1990
        Took out insert/delete char again: it looks awful on X terminals.
--- Rob Duncan, Apr 23 1990
        Reinstated insert and delete line capabilities, plus insert and
        delete char (it does make a difference).
        Set -vedscreenwrap- for safety.
--- Rob Duncan, Feb  2 1990
        Changed to use new -vedresize- in place of -vedwin_adjust-.
        Removed assignments to -vedstartwindow- (now adjusted automatically)
--- Rob Duncan, Jan 17 1990
        Undid last change (modified vt100 instead)
--- Rob Duncan, Jan 16 1990
        Added insert and delete line capabilities for xterm screen.
--- Andreas Schoter & Rob Duncan, Aug-Oct 1989
        Renamed file from "vedxtermcore.p"and added -vedxtermscreen- so that
        it can be loaded automatically by -veduseterm-.
        Moved out everything not related to screen control to "vedxtermkeys".
 */
