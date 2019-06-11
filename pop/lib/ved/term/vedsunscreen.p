/* --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.all/lib/ved/term/vedsunscreen.p
 > Purpose:        Configure VED for Sun console or sunview window
 > Author:         Ben Rubinstein, Mar  6 1986 (see revisions)
 > Documentation:  HELP *VEDSUN
 > Related Files:  LIB *VEDSUN *VEDSUNKEYS *VEDWINSUN
 */
compile_mode :pop11 +strict;

uses-now popsunlib;     ;;; get $popsunlib in path, to get vedwinsunprox
uses vedwinsunprox;     ;;; useful procedures for resizing windows

section;

vars
    vedsuninwindow = false,
        ;;; set <true> when running in a sunview window
    procedure vedsunwindowsize,
        ;;; returns the dimensions of the current window
    procedure vedsunrefresh,
        ;;; refreshes the screen
;

;;; default_tty_size:
;;;     default size of a sun window

define lconstant default_tty_size();
    #_< {80 34} >_#;
enddefine;

default_tty_size -> vedsunwindowsize;


;;; inwindow:
;;;     tests whether we're running in a sunview window

define lconstant inwindow();
    lvars   s;
    dlocal  vvedscreensendidseq = '\^[[11t', vedescape = `\^[`;
    vedsuninwindow
    or systranslate('WINDOW_ME')
    or (vedtermidseq() ->> s)
    and (s = '[1t' /* open window */ or s = '[2t' /* closed window */);
enddefine;

;;; vedwinsuninit:
;;;     obtains the current window size and resets VED to fit

define lconstant vedwinsuninit();
    lvars v, w, h;
    if vedwin_call5(vedsunwindowsize) ->> v then
        explode(v) -> h -> w;
        unless vedchecksize(h, w) then
            vedresize(h, w, false);
        endunless;
    endif;
enddefine;

;;; vedwinsunrefresh:
;;;     refreshes the screen, accounting for possible window resize

define lconstant vedwinsunrefresh();
    lvars v, w, h;
    if vedsunwindowsize() ->> v then
        explode(v) -> h -> w;
        returnunless(vedchecksize(h, w))(vedresize(h, w));
    endif;
    vedrefresh();
enddefine;

;;; vedsunscreen:
;;;     configures VED for Sun console or shelltool window

define vedsunscreen();

    "sun" -> vedterminalname;
    false -> vedterminalselect;

    ;;; default window size
    80 -> vedscreenwidth;
    34 -> vedscreenlength;
    true -> vedscreenwrap;

    ;;; enable insert & delete for lines but not characters: too slow
    false -> vednolineinsert;
    false -> vednolinedelete;

    ;;; disable scrolling: too slow
    false -> vedscrollscreen;

    ;;; cursor motion
    vedansiscreenxy -> vedscreenxy;

    ;;; make -vedwiggle- noticeable
    12 -> vedwiggletimes;

    vedset screen
        charup          = esc [ A
        chardown        = esc [ B
        charleft        = esc [ D
        charright       = esc [ C
        insertline      = esc [ L
        deleteline      = esc [ M
        deletechar      = esc [ P
        cleartail       = esc [ K
        clear           = ^L
        charnormal      = esc [ m
        charaltfont     = esc [ 7 m
        charhighlight   = esc [ 7 m
    endvedset;

    ;;; if running in a sunview window, allow for possible window resizing
    if inwindow() then
        vedwin_tty_size -> vedsunwindowsize;
        vedwinsuninit -> vvedscreeninit;
        vedwinsunrefresh -> vedsunrefresh;
    else
        default_tty_size -> vedsunwindowsize;
        nullstring -> vvedscreeninit;
        vedrefresh -> vedsunrefresh;
    endif;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 14 1992
        call5 -> vedwin_call5
--- John Gibson, Feb 14 1992
        Removed vedset g*raphic
--- Robert John Duncan, Jan  7 1992
        Added character display attributes
--- Robert John Duncan, Oct  3 1990
        Changed -vedwinsuninit- and -vedwinsunrefresh- to use -vedchecksize-
--- Rob Duncan, Feb  2 1990
        Extended -inwindow- to interrogate the terminal to distinguish
        between console and shelltool window.
        Added redefinable -vedsunwindowsize- to fix bug in VEDCMDTOOL
        which was getting the window size wrong.
--- Rob Duncan, Nov  2 1989
        Added global var -vedsuninwindow- to simplify interface to
        VEDWINSUN and VEDCMDTOOL
--- Rob Duncan, Oct 26 1989
        Merged with VEDWINSUN, using procedure -inwindow- to test whether to
        allow for window size changes
--- Rob Duncan, Oct 13 1989
    Tidied up: removed a lot of stuff made redundant by the new VED setup
    procedure and a lot of meaningless comments.
--- Jason Handby, Jul 13 1989
    Put everything inside definition of -vedsunscreen-, changed to use
    vedset notation, moved -vedwiggletimes- assignment into this file
--- Robert Duncan, Sep  1 1987
    reduced the values of -poplinemax- and -poplinewidth- so that long lines
    produced by LMR and IM get broken without the screen being redrawn first.
    See bugreport robd.1
--- Ben Rubinstein, Nov 28 1986 - removed references to sun2 to avoid distress
--- Ben Rubinstein, Nov 20 1986 - moved status header setup to ..keys.p
*/
