/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
> File:           C.all/lib/ved/term/vedsunkeys_SUNVIEW.p
> Purpose:        VED key mappings for Sun with accelerator keys ON
> Author:         Ben Rubinstein, Aug 15 1986 (see revisions)
> Documentation:  HELP * VEDSUN, HELP * SUNKEYS_SUNVIEW
> Related Files:  LIB * VEDSUNKEYS, LIB * vedsunkeys_NOSUNVIEW
*/
compile_mode :pop11 +strict;

section;

define vedsunkeys_SUNVIEW();

    vedset keys;
        charup            = esc [ A
        chardown          = esc [ B
        charright         = esc [ C
        charleft          = esc [ D
        charuplots        = esc esc [ A
        chardownlots      = esc esc [ B
        charrightlots     = esc esc [ C
        charleftlots      = esc esc [ D
        charupleftlots    = esc esc [ 2 1 4 z               ;;; ESC R7
        charuprightlots   = esc esc [ 2 1 6 z               ;;; ESC R9
        chardownleftlots  = esc esc [ 2 2 0 z               ;;; ESC R13
        chardownrightlots = esc esc [ 2 2 2 z               ;;; ESC R15
        wordleft          = esc [ 2 1 4 z                   ;;; R7
        wordleft          = esc [ 2 2 0 z                   ;;; R13
        wordright         = esc [ 2 1 6 z                   ;;; R9
        textright         = esc [ 2 1 3 z                   ;;; R6
        lineabove         = esc esc [ 2 0 8 z               ;;; ESC R1
        linebelow         = esc esc [ 2 1 0 z               ;;; ESC R3
        screenup          = esc [ 2 0 9 z                   ;;; R2
        screenleft        = esc [ 2 1 1 z                   ;;; R4
        screendown        = esc [ 2 1 2 z                   ;;; R5
        topfile           = esc [ 2 0 8 z                   ;;; R1
        endfile           = esc [ 2 1 0 z                   ;;; R3
        clearhead         = esc [ 2 2 6 z                   ;;; F3
        cleartail         = esc [ 2 2 8 z                   ;;; F5
        linedelete        = esc [ 2 2 7 z                   ;;; F4
        ident enterkey    = esc [ 2 2 2 z                   ;;; R15
        pushkey           = esc [ 1 9 3 z                   ;;; L2
        ident redokey     = esc [ 1 9 4 z                   ;;; L3
        popkey            = esc [ 1 9 5 z                   ;;; L4
        "ENTER yankw"     = esc esc [ 2 2 5 z               ;;; ESC F2
        "ENTER yankw"     = esc esc [ 2 2 6 z               ;;; ESC F3
        "ENTER yankl"     = esc esc [ 2 2 7 z               ;;; ESC F4
        "ENTER yankw"     = esc esc [ 2 2 8 z               ;;; ESC F5
        ENTER m           = esc [ 2 3 1 z                   ;;; F8
        ENTER mi          = esc esc [ 2 3 1 z               ;;; ESC F8
        ENTER t           = esc [ 2 3 2 z                   ;;; F9
        ENTER ti          = esc esc [ 2 3 2 z               ;;; ESC F9
        marklo            = esc [ 2 2 9 z                   ;;; F6
        markfind          = esc esc [ 2 2 9 z               ;;; ESC F6
        markhi            = esc [ 2 3 0 z                   ;;; F7
        "endrange"        = esc esc [ 2 3 0 z               ;;; ESC F7
        "midwindow"       = esc [ 2 1 8 z                   ;;; R11
        switchstatus      = esc [ 2 2 5 z                   ;;; F2

        /* Extra bindings for Type 4 keyboard */

        wordleft            =   esc [ 2 4 7 z       ;;; KP 0
        wordright           =   esc [ 2 4 9 z       ;;; KP .
        ident enterkey      =   esc [ 2 5 0 z       ;;; Enter
        statusswitch        =   esc [ 2 5 3 z       ;;; KP +
        ident redokey       =   esc [ 2 5 4 z       ;;; KP -
        "helpkey"           =   esc [ 2 0 7 z       ;;; Help

    endvedset;

    'sunkeys_sunview' -> vedkeymapname
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan  5 1993
        Replaced quoted words with idents for some core variable ops
--- Robert John Duncan, Jun 21 1991
        Added missing binding for R13
--- John Williams, Oct 17 1990
        Defines -vedsunkeys_SUNVIEW- instead of -vedsunkeys-
--- John Williams, Oct 12 1990
        Added extra key bindings for Type 4 keyboard
        Now sets -vedkeymapname-
--- Rob Duncan, Oct 25 1989
        Moved from POPSUNLIB to VEDTERMLIB (POPVEDLIB/term)
--- Rob Duncan, Oct 13 1989
        Removed definition and use of -vedsunscroll-
--- Jason Handby, Jul 12 1989 - changed to use vedset notation, moved
        screen stuff to vedsunscreen.p.
--- Ben Rubinstein, Oct 12 1986 - vedenter, vedredo indirected through ..key
*/
