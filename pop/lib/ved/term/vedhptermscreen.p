/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedhptermscreen.p
 > Purpose:         VED: Screen setup for HP terminal windows
 > Author:          Julian Clinton, October 1991 (see revisions)
 > Documentation:
 > Related Files:   LIB *VEDHPSCREEN, *VEDHPTERMKEYS
 */
compile_mode :pop11 +strict;

section;

uses vedhpscreen;

include sysdefs

lconstant TIOCGWINSZ =
    ;;; ioctl command for obtaining the tty window size
#_IF DEF HPUX
    16:4008746B
#_ELSEIF DEF SUNOS or DEF ULTRIX
    16:40087468
#_ELSE
    false
#_ENDIF;

define vedhptermscreen();

    define lconstant hptermscreeninit();
        ;;; Use ioctl(2) to interrogate the tty driver
        defclass lconstant winsize {
        >-> ws_row      : ushort,
            ws_col      : ushort,
            ws_xpixel   : ushort,
            ws_ypixel   : ushort,
        };
        lconstant buff = writeable conswinsize(0,0,0,0);
        if DEF TIOCGWINSZ
        and sys_io_control(popdevin, TIOCGWINSZ, buff)
        then
            unless vedchecksize(ws_row(buff), ws_col(buff)) then
                vedresize(ws_row(buff), ws_col(buff), false);
            endunless;
        endif;
    enddefine;

    vedhpscreen();
    ;;; hpterm should set LINES & COLUMNS in the environment
    lvars n;
    if systranslate('LINES') ->> n then
        if strnumber(n) ->> n then
            n -> vedscreenlength;
        endif;
    endif;
    if systranslate('COLUMNS') ->> n then
        if strnumber(n) ->> n then
            n -> vedscreenwidth;
        endif;
    endif;
    hptermscreeninit -> vvedscreeninit;
    "hpterm" -> vedterminalname;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Mar  3 1995
        Removed references to defunct systems
--- Robert Duncan, May 11 1993
        Added code to get the window size from the tty driver
--- Robert John Duncan, Oct 15 1992
        Added check for LINES and COLUMNS environment variables
 */
