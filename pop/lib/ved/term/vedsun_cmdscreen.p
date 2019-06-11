/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedsun_cmdscreen.p
 > Purpose:         VED: Screen setup for Sun cmdtool window
 > Author:          Rob Duncan, Oct 27 1989 (see revisions)
 > Documentation:
 > Related Files:
 */

compile_mode:pop11 +strict;

section;

include sysdefs
uses vedsunscreen

define lconstant windowsize() -> ws;
    lvars ws;
#_IF DEF SUNOS or DEF ULTRIX
    ;;; Use ioctl(2) to interrogate the tty driver
    defclass lconstant winsize {
    >-> ws_row      : ushort,
        ws_col      : ushort,
        ws_xpixel   : ushort,
        ws_ypixel   : ushort,
    };
    lconstant buff = writeable conswinsize(0,0,0,0);
  #_IF DEFV SUNOS >= 5.0
    lconstant TIOCGWINSZ = (`T`<<8)||104;
  #_ELSE
    lconstant TIOCGWINSZ = 16:40087468;
  #_ENDIF
    if sys_io_control(popdevin, TIOCGWINSZ, buff) ->> ws then
        {% buff.ws_col, buff.ws_row %} -> ws;
    endif;
#_ELSE
    false -> ws;
#_ENDIF
enddefine;

define global vedsun_cmdscreen();
    dlocal vedsuninwindow = true;
    vedsunscreen();
/*
    vedset screen
        charbold        = esc [ 1 m
    endvedset;
*/

    "'sun-cmd'" -> vedterminalname;
    ;;; Procedure to determine window size
    procedure() -> v;
        lvars v;
        unless windowsize() ->> v then
            if vedwin_tty_size() ->> v then
                ;;; Change to account for two columns of scroll-bar
                v(1) - 2 -> v(1);
            endif;
        endunless;
    endprocedure -> vedsunwindowsize;
    ;;; Change screen init/reset sequences to switch to/from editing mode
    procedure(old);
        lvars old;
        vedscreencontrol('\^[[>4l');
        vedscreencontrol(old);
    endprocedure(% vvedscreeninit %) -> vvedscreeninit;
    procedure(old);
        lvars old;
        vedscreencontrol(old);
        vedscreencontrol('\^[[>4h');
    endprocedure(% vvedscreenreset %) -> vvedscreenreset;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jun 22 1995
        Fixed winsize ioctl for Solaris
--- Robert John Duncan, Mar  3 1995
        Removed references to defunct systems
--- Robert John Duncan, Jul 26 1991
        Another window-size fix: commandtool under OpenWindows returns
        completely the wrong number of columns. Added -windowsize-
        procedure to interrogate the tty using an ioctl.
--- Rob Duncan, Feb  2 1990
        Fixed to get the window size right: the window size returned by
        -vedwin_tty_size- is two columns wider than the available screen
        because of space left for the scroll bar.
        Made screen init/reset sequences more sophisticated to allow for
        values assigned by -vedsunscreen-
--- Rob Duncan, Nov  2 1989
        Added dlocal -vedsuninwindow-
 */
