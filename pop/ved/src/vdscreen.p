/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/ved/src/vdscreen.p
 > Purpose:
 > Author:          John Gibson & Aaron Sloman (see revisions)
 */

#_INCLUDE 'vddeclare.ph'

global constant
        procedure (vedscreencontrol, Sys$-Ved$-Clear_statusline)
    ;

global vars
        vedinserting, vedscreennormalcursor, vedscreenstaticcursor
    ;


;;; ------------------------------------------------------------------------

section $-Sys$-Ved =>
                vedscreenline, vedscreencolumn,
                vedscreencharup, vedscreenchardown, vedscreencharleft,
                vedscreencharright, vedscreenscreenleft,
                vedvt52screenxy, vedansiscreenxy, vedscreenxy,
                vedscreencursoron,
                vedscreenclear, vedscreencleartail,
                vedscreeninsertline, vedscreendeleteline,
                vedscreeninsertchar, vedscreendeletechar,
                vedansisetscrollregion, vedsetscrollregion,
                vedscreenscrollregionup, vedscreenscrollregiondown,
                vedscreenpulldown, vedscreenpushup, vedscreenpushdown,
                vedscreenpullup,
            ;

vars
    vedscreenline   = MAXSCREENCOL,
    vedscreencolumn = MAXSCREENCOL,

    terminal_can_scroll = false,
    ;


;;; --- SCREEN CURSOR MOVEMENT ---------------------------------------------

;;; vedscreencharup, down, left, right:
;;;     basic cursor movement

define vars vedscreencharup();
    vedscreencontrol(vvedscreencharup);
    vedscreenline fi_- 1 -> vedscreenline;
enddefine;

define vars vedscreenchardown();
    vedscreencontrol(vvedscreenchardown);
    vedscreenline  fi_+  1 -> vedscreenline;
enddefine;

define vars vedscreencharleft();
    vedscreencontrol(vvedscreencharleft);
    vedscreencolumn fi_- 1 -> vedscreencolumn;
enddefine;

define vars vedscreencharright();
    vedscreencontrol(vvedscreencharright);
    vedscreencolumn  fi_+  1 -> vedscreencolumn;
enddefine;

define vedscreenscreenleft();
    vedscreencontrol(vvedscreenscreenleft);
    1 -> vedscreencolumn;
enddefine;

;;; vedscreenxy:
;;;     moves the cursor to a given screen position. Lines and columns are
;;;     numbered from 1 and start at the top left.
;;;     -vedvt52screenxy- and -vedansiscreenxy- are two possible versions.
;;;     -Undef_screenxy- is the default, and must be replaced by the
;;;     terminal setup code.

define Undef_screenxy(col, line) with_props vedscreenxy;
    lvars col, line;
    mishap(0, 'CURSOR MOTION NOT DEFINED');
enddefine;

define vedvt52screenxy(col, line) with_props vedscreenxy;
    lvars col, line;
    vedscreencontrol(vvedscreenpoint);
    vedscr_char_out(31  fi_+  line);
    vedscr_char_out(31  fi_+  col);
    col -> vedscreencolumn;
    line -> vedscreenline;
enddefine;

define vedansiscreenxy(col, line) with_props vedscreenxy;
    lvars   col, line;
    dlocal  cucharout = vedscr_char_out, pop_pr_radix = 10;
    vedscr_char_out('\^[[');
    sys_syspr(line);
    vedscr_char_out(`;`);
    sys_syspr(col);
    vedscr_char_out(`H`);
    col -> vedscreencolumn;
    line -> vedscreenline;
enddefine;

vars procedure vedscreenxy = Undef_screenxy;


;;; --- SCREEN CURSOR  ----------------------------------------------------

lvars cursor_char = `O`;

define Default_screencursoron() with_props vedscreencursoron;
    cursor_char
enddefine;
;;;
define updaterof Default_screencursoron(cchar) with_props vedscreencursoron;
    lvars cchar;
    if cchar == true then `O` else cchar endif -> cursor_char
enddefine;

vars active vedscreencursoron = Default_screencursoron;

define Set_wait_cursor(ensure_on, set_pos);
    lvars ensure_on, set_pos, curs, char;
    if set_pos then vedsetcursor() endif;
    vedscr_flush_output();
    0 -> vedscreencharmode;

    vedscreencursoron -> curs;
    returnunless(curs or ensure_on);
    if isinteger(ensure_on) then
        ensure_on
    elseif vedstatic then
        if isinteger(vedstatic) and vedstatic &&/=_0 2:1e1 then
            ;;; bit 1 set in vedstatic means use transparent cursor
            `\s`
        else
            vedscreenstaticcursor
        endif
    else
        vedscreennormalcursor
    endif -> char;
    if curs /== char then char -> vedscreencursoron endif
enddefine;


;;; --- SCREEN CLEAR -------------------------------------------------------

define vars vedscreenclear();
    vedscreencontrol(vvedscreenclear);
    1 -> vedscreencolumn;
    1 -> vedscreenline;
enddefine;

define vars vedscreencleartail();
    lvars string, scrcol, len;
    if vedonstatus then
        vedstatusline -> string;
        vedscreencolumn -> scrcol;
        datalength(string) fi_- scrcol fi_+ 1 -> len;
        if len fi_> 0 then Set_subvedstring(`\s`, scrcol, string, len) endif
    endif;
    vedscreencontrol(vvedscreencleartail)
enddefine;


;;; --- SCREEN INSERT/DELETE ----------------------------------------------

;;; vedscreeninsertline, vedscreendeleteline:
;;;     insert/delete line on screen. -wline- is the window line number

define vars vedscreeninsertline(wline);
    lvars wline;
    if vednolineinsert then
        Set_refresh_needed(wline);
    else
        vedwindowpoint(wline, fi_min(vedscreenwidth, vedscreencolumn));
        vedscreencontrol(vvedscreeninsertline);
        MAXSCREENCOL ->> vedscreenline -> vedscreencolumn
    endif
enddefine;

define vars vedscreendeleteline(wline);
    lvars wline;
    if vednolinedelete then
        Set_refresh_needed(wline);
    else
        unless wline fi_+ vedscreenoffset == vedscreenline then
            vedwindowpoint(wline, 1);
        endunless;
        vedscreencontrol(vvedscreendeleteline);
    endif
enddefine;

;;; vedscreeninsertchar, vedscreendeletechar:
;;;     insert/delete character at the current cursor position.
;;;     -vedscreeninsertchar- turns insert mode ON but not OFF: this is
;;;     to optimise insertion of multiple characters

define vars vedscreeninsertchar(/* c */) with_nargs 1;
    unless vedinserting then
        0 -> vedscreencharmode;
        vedscreencontrol(vvedscreeninsertmode);
        true -> vedinserting;
    endunless;
    vedscreencontrol(vvedscreeninsertchar);
    vedscreenoutput(/* c */);
enddefine;

define vars vedscreendeletechar();
    0 -> vedscreencharmode;
    if vedonstatus then
        Shift_string(vedscreencolumn, -1, vedstatusline, false) ->
    endif;
    vedscreencontrol(vvedscreendeletechar);
enddefine;


;;; --- SCREEN SCROLLING ---------------------------------------------------

;;; vedsetscrollregion:
;;;     sets the terminal scrolling region between lines -top- and -bottom-.
;;;     -vedansisetscrollregion- is one possible value;
;;;     -Undef_setscrollregion- is the default which must be replaced by
;;;     the terminal setup code.

define vedansisetscrollregion(top, bottom) with_props vedsetscrollregion;
    lvars   top, bottom;
    dlocal  cucharout = vedscr_char_out, pop_pr_radix = 10;
    vedscr_char_out('\^[[');
    sys_syspr(top);
    vedscr_char_out(`;`);
    unless bottom == vedscreenlength then
        sys_syspr(bottom);
    endunless;
    vedscr_char_out(`r`);
    MAXSCREENCOL ->> vedscreenline -> vedscreencolumn;
enddefine;

define Undef_setscrollregion(top, bottom) with_props vedsetscrollregion;
    lvars top, bottom;
    mishap(0, 'SCROLLING REGIONS NOT DEFINED');
enddefine;

vars procedure vedsetscrollregion = Undef_setscrollregion;

;;; vedscreenscrollregionup, vedscreenscrollregiondown:
;;;     scroll a region between window lines -wtop- and -wbottom-

define vars vedscreenscrollregionup(wtop, wbottom);
    lvars wtop, wbottom;
    returnunless(vedediting);
    if wtop == wbottom then
        ;;; scrolling one line only: just clear it
        vedscreenxy(1, wtop fi_+ vedscreenoffset);
        vedscreencleartail();
        MAXSCREENCOL ->> vedscreenline -> vedscreencolumn;
    elseif terminal_can_scroll == 1 then
        ;;; terminal supports scrolling regions
        vedsetscrollregion(wtop fi_+ vedscreenoffset, wbottom fi_+ vedscreenoffset);
        vedscreenxy(1, wbottom fi_+ vedscreenoffset);
        vedscreencontrol(vvedscreenscrollup);
        ;;; reset scroll region to whole screen
        vedsetscrollregion(1, vedscreenlength);
        MAXSCREENCOL ->> vedscreenline -> vedscreencolumn;
    else
        vedscreendeleteline(wtop);
        unless wbottom fi_+ vedscreenoffset == vedscreenlength then
            vedscreeninsertline(wbottom);
        endunless;
    endif;
enddefine;

define vars vedscreenscrollregiondown(wtop, wbottom);
    lvars wtop, wbottom;
    returnunless(vedediting);
    if wtop == wbottom then
        ;;; scrolling one line only: just clear it
        vedscreenxy(1, wtop fi_+ vedscreenoffset);
        vedscreencleartail();
        MAXSCREENCOL ->> vedscreenline -> vedscreencolumn;
    elseif terminal_can_scroll == 1 then
        ;;; terminal supports scrolling regions
        vedsetscrollregion(wtop fi_+ vedscreenoffset, wbottom fi_+ vedscreenoffset);
        vedscreenxy(1, wtop fi_+ vedscreenoffset);
        vedscreencontrol(vvedscreenscrolldown);
        ;;; reset scroll region to whole screen
        vedsetscrollregion(1, vedscreenlength);
        MAXSCREENCOL ->> vedscreenline -> vedscreencolumn;
    else
        unless wbottom fi_+ vedscreenoffset == vedscreenlength then
            vedscreendeleteline(wbottom);
        endunless;
        vedscreeninsertline(wtop);
    endif;
enddefine;

define vars vedscreenpulldown(wline);
    lvars wline;
    returnunless(vedediting);
    if terminal_can_scroll then
        vedscreenscrollregiondown(2, wline);
    elseif wline == vedscreenlength and vvedscreenscrolldown /= nullstring
    then
        vedwindowpoint(1, 1);
        vedscreencontrol(vvedscreencleartail);
        vedscreencontrol(vvedscreenscrolldown);
        Clear_statusline();
    else
        Set_refresh_needed(2);
    endif;
enddefine;

define vars vedscreenpushup(wline);
    lvars wline;
    returnunless(vedediting);
    if terminal_can_scroll then
        vedscreenscrollregionup(2, wline);
    elseif wline == vedscreenlength and vvedscreenscrollup /= nullstring
    then
        vedwindowpoint(wline, 1);
        vedscreencontrol(vvedscreenscrollup);
        Clear_statusline();
    else
        Set_refresh_needed(2);
    endif;
enddefine;

define vars vedscreenpushdown(wline);
    lvars wline;
    returnunless(vedediting);
    vedscreenscrollregiondown(wline, vedwindowlength);
enddefine;

define vars vedscreenpullup(wline);
    lvars wline;
    returnunless(vedediting);
    vedscreenscrollregionup(wline, vedwindowlength);
enddefine;

endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 17 1996
        Changed Set_wait_cursor to set transparent cursor `\s` if bit 1 set
        in vedstatic.
--- John Gibson, Sep 13 1995
        Changed to use Set_subvedstring
--- John Gibson, Jan 13 1994
        Added Set_wait_cursor
--- John Gibson, Jan  4 1992
        Changed set_b*ytes to set_sub*vector
--- John Gibson, Dec 20 1991
        Replaced vedscreeng*raphoff() with 0 -> vedscreencharmode
--- John Gibson, Jul 16 1991
        Made vedstatusline updated BEFORE doing screen operation
        in -vedscreendeletechar-
--- John Gibson, Jul  9 1991
        Sectionised; added vedscreencursoron.
--- John Gibson, Apr  9 1991
        Uses vedscr_char_out.
--- Rob Duncan, Apr  5 1990
        Shortened sequence output by -vedansisetscrollregion- when region
        is the whole screen.
--- Rob Duncan, Nov  7 1989
        Changed -vedscreencharleft-, -vedscreenchardown-, -vedscreenscreenleft-
        to use corresponding -vvedscreen*- variables instead of assuming
        `\n`, `\b`, `\r`;
        moved -vedansiscreenxy- in from "vdvt100.p"; added -vedundefscreenxy-
        as new default;
        added vars procedure -vedsetscrollregion- and created -vedansisetscrollregion-
        from code in "vdvt100.p";
        changed -vedscreenscrollregionup/down- to look at -terminal_can_scroll-
        and use -vedsetscrollregion- where possible;
        extended -vedscreeninsertchar- to output -vvedscreeninsertchar- for
        terminals with "insert single character" ability;
        replaced -vedscreenescape- throughout by -vedscreencontrol-;
        tidied up.
 */
