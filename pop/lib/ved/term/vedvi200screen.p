/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvi200screen.p
 > Purpose:         set the screen environment of vi200 terminal
 > Author:          Aaron Sloman & John Gibson (see revisions)
 > Related Files:   vedvi200.p vedvi200keys.p
 */
compile_mode :pop11 +strict;

section;

;;; Unprotect "vedvi200screen" in case we're loading this file in a system
;;; where it's already built in
sysunprotect("vedvi200screen");

define vedvi200screen();

    "vi200"         -> vedterminalname;
    false           -> vedterminalselect;
    false           -> vednokeypad;
    false           -> vednocharinsert;
    false           -> vednolineinsert;
    false           -> vednochardelete;
    false           -> vednolinedelete;
    vedvt52screenxy -> vedscreenxy;
    vedvt52screengraphtrans -> vedscreengraphtrans;


;;; --- VDU CONTROL CODES ------------------------------------------------

    vedset screen
    ;;; small moves
        charleft            = esc D
        charright           = esc C
        charup              = esc A
        chardown            = ^J

    ;;; small delete
        deletechar          = esc O

    ;;; big deletes
        clear               = esc v
        cleartail           = esc x
        deleteline          = esc M

    ;;; miscellaneous
        alpha               = esc G
        graphic             = esc F
        insertline          = esc L
        insertmode          = esc i
        overmode            = esc j
        point               = esc Y
        charnormal          = esc 3
        charaltfont         = esc 4

    ;;; The next two are used with Visual 200 terminals to cause VEDREFRESH
    ;;; to transmit a sequence of characters which unset various special modes
    ;;; which may have accidentally been set up
    ;;; lock block mode key, reset protect screen, reset Auto tab,
    ;;; reset hold screen, switch off block mode
        setpad              = '\^[=\^[;\^[7\^[S\^[\\\^[l\^[n'
        resetpad            = '\^[>\^[;\^[7\^[S\^[\\\^[l\^[n'

    ;;; escape sequence to elicit identity sequence from terminal
        sendidseq           = esc Z

    endvedset;

    ;;;assign the correct procedures to initialise the terminal
    vedscreencontrol(% vvedscreensetpad %)   -> vvedscreeninit;
    vedscreencontrol(% vvedscreenresetpad %) -> vvedscreenreset;

enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb  7 1992
        Added assignment to vedscreengraphtrans and removed assignments
        for graphic chars
--- Robert John Duncan, Jan  7 1992
        Added character display attributes
--- Rob Duncan, Oct  9 1989
        Sectionised
--- Andreas Schoter, Jul 27 1989
        Modified to work with VED's new defaults
--- Andreas Schoter, Jul 12 1989
        Modified to use vedset syntax and screen stuff seperated out
--- John Gibson, Dec 16 1987
        Tidied up.
 */
