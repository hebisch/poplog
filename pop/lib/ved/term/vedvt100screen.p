/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvt100screen.p
 > Purpose:         set the screen environment of vt100 terminal
 > Author:          Steve Hardy & John Gibson (see revisions)
 > Related Files:   vedvt100.p vedvt100keys.p
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING  ;;; because already defined in core system

section;

;;; Unprotect "vedvt100screen" in case we're loading this in a system
;;; where it's built in
sysunprotect("vedvt100screen");

define vedvt100screen();
;;; general control variable assignments
    "vt100"                -> vedterminalname;
    false                  -> vedterminalselect;
    false                  -> vednokeypad;
    vedansiscreenxy        -> vedscreenxy;
    vedansisetscrollregion -> vedsetscrollregion;
    ;;; translation for VED standard graphic symbols
    vedvt100screengraphtrans -> vedscreengraphtrans;

;;; assign screen control variables
    vedset screen
    ;;; character movement
        charleft        = esc [ D
        charright       = esc [ C
        charup          = esc [ A

    ;;; big moves
        scrollup        = esc D
        scrolldown      = esc M

    ;;; delete codes
        clear           = esc [ 2 J
        cleartail       = esc [ K

    ;;; terminal control strings
        setpad          = '\^[<\^[=\^[[?4l'
        resetpad        = esc >
        graphic         = esc ( 0
        alpha           = esc ( B
        charnormal      = esc [ m
        charbold        = esc [ 1 m
        charunderline   = esc [ 4 m
        charblink       = esc [ 5 m
        charhighlight   = esc [ 7 m
    endvedset;

enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb  8 1992
        Added assignment to vedscreengraphtrans and removed setting of
        graph char variables
--- John Gibson, Jan 28 1992
        Addded charblink
--- Robert John Duncan, Jan  7 1992
        Added character display attributes
--- Rob Duncan, Oct  9 1989
        Sectionised
--- Andreas Schoter, Jul 27 1989
        Modified to work with VED's new defaults
--- Andreas Schoter, Jul 13 1989
        Modified to use vedset notation and split into screen and key files
--- John Gibson, Aug 16 1987
        Lconstant'ed, etc.
 */
