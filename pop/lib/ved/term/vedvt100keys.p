/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvt100keys.p
 > Purpose:         set the keybindings for vt100 terminal
 > Author:          Steve Hardy & John Gibson (see revisions)
 > Related Files:   vedvt100screen.p vedvt100.p
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING  ;;; because already defined in core system

section;

;;; Unprotect "vedvt100keys" in case we're loading this in a system where
;;; it's already built-in
sysunprotect("vedvt100keys");

define vedvt100keys();

    vedset keys
    ;;; for ESC [ <char>
        screenup    = esc [ A
        screendown  = esc [ B
        textright   = esc [ C
        screenleft  = esc [ D

    ;;;gold table entries (gold key transmits 'esc O P' )
        refresh                = esc O P esc O P
        lineabove              = esc O P esc O Q
        linebelow              = esc O P esc O R
        setstatic              = esc O P esc O S
        chardelete             = esc O P esc O w
        dotdelete              = esc O P esc O x
        charright <> dotdelete = esc O P esc O y
        clearhead              = esc O P esc O t
        linedelete             = esc O P esc O u
        cleartail              = esc O P esc O v
        pushkey                = esc O P esc O q
        popkey                 = esc O P esc O r
        exchangeposition       = esc O P esc O s
        wordleftdelete         = esc O P esc O p
        wordrightdelete        = esc O P esc O n
        topfile                = esc O P esc [ A
        endfile                = esc O P esc [ B
        dotdelete              = esc O P del

    ;;; mapping between ESC ESC O char and procedures
        chardownleftlots       = esc esc O q
        chardownlots           = esc esc O r
        chardownrightlots      = esc esc O s
        charleftlots           = esc esc O t
        charmiddle             = esc esc O u
        charrightlots          = esc esc O v
        charupleftlots         = esc esc O w
        charuplots             = esc esc O x
        charuprightlots        = esc esc O y

    ;;; mapping between  ESC O <char> and procedures
        screenup               = esc O A
        screendown             = esc O B
        textright              = esc O C
        screenleft             = esc O D
        charleft               = esc O t
        charmiddle             = esc O u
        charright              = esc O v
        statusswitch           = esc O l
        wordleft               = esc O p
        wordright              = esc O n
        "enterkey"             = esc O M
        "redokey"              = esc O m
        chardownleft           = esc O q
        chardown               = esc O r
        chardownright          = esc O s
        charupleft             = esc O w
        charup                 = esc O x
        charupright            = esc O y
        marklo                 = esc O Q
        markhi                 = esc O R
        "ENTER m"              = esc O S

    endvedset;

enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Oct 26 1995
        Removed mapping from ESC P to vedrefresh (BR isl-fr.4548).
--- Rob Duncan, Oct  9 1989
        Sectionised
--- Andreas Schoter, Jul 13 1989
        Modified to use vedset notation and split into screen and key files
--- John Gibson, Aug 16 1987
        Lconstant'ed, etc.
 */
