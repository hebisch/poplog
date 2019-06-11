/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:           C.all/lib/ved/term/vedsunkeys_NOSUNVIEW.p
 > Purpose:        VED key mappings for Sun with accelerator keys OFF
 > Author:         Ben Rubinstein, Apr  1 1986 (see revisions)
 > Documentation:  HELP * VEDSUN, HELP * SUNKEYS
 > Related Files:  LIB * VEDSUNKEYS, LIB * vedsunkeys_SUNVIEW
 */
compile_mode :pop11 +strict;

section;

define vedsunkeys_NOSUNVIEW();
    vedset keys

    /* Left hand keypad */

        lineabove           =   esc [ 2 3 4 z       ;;; L2
        pushkey             =   esc [ 2 0 3 z       ;;; F3
        linebelow           =   esc [ 2 3 5 z       ;;; L4
        exchangeposition    =   esc [ 2 0 4 z       ;;; L5
        "loadline"          =   esc [ 2 3 6 z       ;;; L6
        popkey              =   esc [ 2 0 5 z       ;;; L7
        ident redokey       =   esc [ 2 3 7 z       ;;; L8
        statusswitch        =   esc [ 2 0 6 z       ;;; L9
        ident enterkey      =   esc [ 2 3 8 z       ;;; L10

        "ENTER crm"         =   esc esc [ 2 3 4 z   ;;; ESC L2
        "ENTER break"       =   esc esc [ 2 0 3 z   ;;; ESC L3
        "sunrefresh"        =   esc esc [ 2 3 5 z   ;;; ESC L4
        "ENTER jp"          =   esc esc [ 2 0 4 z   ;;; ESC L5
        setstatic           =   esc esc [ 2 3 6 z   ;;; ESC L6
        ENTER pop           =   esc esc [ 2 0 5 z   ;;; ESC L7
        ident redokey       =   esc esc [ 2 3 7 z   ;;; ESC L8
        switchstatus        =   esc esc [ 2 0 6 z   ;;; ESC L9
        ident enterkey      =   esc esc [ 2 3 8 z   ;;; ESC L10

    /* Function keys */

        marklo              =   esc [ 2 0 2 z       ;;; F1  Type 2
        markhi              =   esc [ 2 2 5 z       ;;; F2
        clearhead           =   esc [ 2 2 6 z       ;;; F3
        linedelete          =   esc [ 2 2 7 z       ;;; F4
        cleartail           =   esc [ 2 2 8 z       ;;; F5
        wordleftdelete      =   esc [ 2 2 9 z       ;;; F6
        wordrightdelete     =   esc [ 2 3 0 z       ;;; F7
        ENTER m             =   esc [ 2 3 1 z       ;;; F8
        ENTER t             =   esc [ 2 3 2 z       ;;; F9
        dotdelete           =   esc [ 2 3 3 z       ;;; BREAK (Type 2 only)

        ENTER mbf           =   esc esc [ 2 0 2 z   ;;; ESC F1  Type 2
        ENTER mef           =   esc esc [ 2 2 5 z   ;;; ESC F2
        "ENTER yankw"       =   esc esc [ 2 2 6 z   ;;; ESC F3
        "ENTER yankl"       =   esc esc [ 2 2 7 z   ;;; ESC F4
        "ENTER yankw"       =   esc esc [ 2 2 8 z   ;;; ESC F5
        "ENTER yankw"       =   esc esc [ 2 2 9 z   ;;; ESC F6
        "ENTER yankw"       =   esc esc [ 2 3 0 z   ;;; ESC F7
        ENTER mi            =   esc esc [ 2 3 1 z   ;;; ESC F8
        ENTER ti            =   esc esc [ 2 3 2 z   ;;; ESC F9

    /* Righthand keypad */

        textleft            =   esc [ 2 0 8 z       ;;; R1
        screenup            =   esc [ 2 0 9 z       ;;; R2
        textright           =   esc [ 2 1 0 z       ;;; R3
        screenleft          =   esc [ 2 1 1 z       ;;; R4
        screendown          =   esc [ 2 1 2 z       ;;; R5
        screenright         =   esc [ 2 1 3 z       ;;; R6
        charupleft          =   esc [ 2 1 4 z       ;;; R7
        charup              =   esc [ A             ;;; R8
        charupright         =   esc [ 2 1 6 z       ;;; R9
        charleft            =   esc [ D             ;;; R10
        "ENTER timed_esc"   =   esc [ 2 1 8 z       ;;; R11
        charright           =   esc [ C             ;;; R12
        wordleft            =   esc [ 2 2 0 z       ;;; R13
        chardown            =   esc [ B             ;;; R14
        wordright           =   esc [ 2 2 2 z       ;;; R15

        "topwindow"         =   esc esc [ 2 0 8 z   ;;; ESC R1
        topfile             =   esc esc [ 2 0 9 z   ;;; ESC R2
        markfind            =   esc esc [ 2 1 0 z   ;;; ESC R3
        "midwindow"         =   esc esc [ 2 1 1 z   ;;; ESC R4
        endfile             =   esc esc [ 2 1 2 z   ;;; ESC R5
        "endrange"          =   esc esc [ 2 1 3 z   ;;; ESC R6
        charupleftlots      =   esc esc [ 2 1 4 z   ;;; ESC R7
        charuplots          =   esc esc [ A         ;;; ESC R8
        charuprightlots     =   esc esc [ 2 1 6 z   ;;; ESC R9
        charleftlots        =   esc esc [ D         ;;; ESC R10
        "midwindow"         =   esc esc [ 2 1 8 z   ;;; ESC R11
        charrightlots       =   esc esc [ C         ;;; ESC R12
        chardownleftlots    =   esc esc [ 2 2 0 z   ;;; ESC R13
        chardownlots        =   esc esc [ B         ;;; ESC R14
        chardownrightlots   =   esc esc [ 2 2 2 z   ;;; ESC R15

        /* Extra bindings for Type 4 keyboard */

        wordleft            =   esc [ 2 4 7 z       ;;; KP 0
        wordright           =   esc [ 2 4 9 z       ;;; KP .
        ident enterkey      =   esc [ 2 5 0 z       ;;; Enter
        statusswitch        =   esc [ 2 5 3 z       ;;; KP +
        ident redokey       =   esc [ 2 5 4 z       ;;; KP -
        "helpkey"           =   esc [ 2 0 7 z       ;;; Help

    endvedset;

    'sun' -> vedkeymapname;
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan  5 1993
        Replaced quoted words with idents for some core variable ops
--- John Williams, Oct 17 1990
        Defines -vedsunkeys_NOSUNVIEW- instead of -vedsunkeys-
--- John Williams, Oct 12 1990
        Added extra bindings for Type 4 keyboard
        Now assigns to -vedkeymapname-
--- John Williams, Oct 11 1990
        Removed old redundant key mappings
--- Rob Duncan, Oct 25 1989
        Moved from POPSUNLIB to POPVEDLIB/term
--- Rob Duncan, Oct 13 1989
        Deleted definitions of procedures which are now autoloadable
            (-vedmidwindow-, -vedtopwindow- etc)
--- Jason Handby, Jul 12 1989
        Changed to use vedset notation, changed name
*/
