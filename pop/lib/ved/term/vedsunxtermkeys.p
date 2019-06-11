/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedsunxtermkeys.p
 > Purpose:         VED keyboard setup for xterm on a sun
 > Author:          Roger Evans, May  7 1989 (see revisions)
 > Documentation:   HELP * VEDXTERM
 > Related Files:   LIB * VEDXTERMKEYS
 */

compile_mode :pop11 +strict;

section;

define vedsunxtermkeys();

#_IF DEF vedxsunfunctionkeys                        ;;; xterm -sf
    vedset keys
        "screenbell"        =   esc [ 1 9 2 z       ;;; L1 (F11)
        lineabove           =   esc [ 1 9 3 z       ;;; L2 (F12)
        pushkey             =   esc [ 1 9 4 z       ;;; F3
        linebelow           =   esc [ 1 9 5 z       ;;; L4
        exchangeposition    =   esc [ 1 9 6 z       ;;; L5
        "loadline"          =   esc [ 1 9 7 z       ;;; L6
        popkey              =   esc [ 1 9 8 z       ;;; L7
        "redokey"           =   esc [ 1 9 9 z       ;;; L8
        statusswitch        =   esc [ 2 0 0 z       ;;; L9
        "enterkey"          =   esc [ 2 0 1 z       ;;; L10

        "screenbell"        =   esc esc [ 1 9 2 z   ;;; ESC L1 (F11)
        "ENTER crm"         =   esc esc [ 1 9 3 z   ;;; ESC L2 (F12)
        "ENTER break"       =   esc esc [ 1 9 4 z   ;;; ESC L3
        "xrefresh"          =   esc esc [ 1 9 5 z   ;;; ESC L4
        ENTER jp            =   esc esc [ 1 9 6 z   ;;; ESC L5
        setstatic           =   esc esc [ 1 9 7 z   ;;; ESC L6
        ENTER pop           =   esc esc [ 1 9 8 z   ;;; ESC L7

        marklo              =   esc [ 2 2 4 z       ;;; F1
        markhi              =   esc [ 2 2 5 z       ;;; F2
        clearhead           =   esc [ 2 2 6 z       ;;; F3
        linedelete          =   esc [ 2 2 7 z       ;;; F4
        cleartail           =   esc [ 2 2 8 z       ;;; F5
        wordleftdelete      =   esc [ 2 2 9 z       ;;; F6
        wordrightdelete     =   esc [ 2 3 0 z       ;;; F7
        ENTER m             =   esc [ 2 3 1 z       ;;; F8
        ENTER t             =   esc [ 2 3 2 z       ;;; F9
        "screenbell"        =   esc [ 2 3 3 z       ;;; F10 (type 4 kbd only)

        ENTER mbf           =   esc esc [ 2 2 4 z   ;;; ESC F1
        ENTER mef           =   esc esc [ 2 2 5 z   ;;; ESC F2
        "ENTER yankw"       =   esc esc [ 2 2 6 z   ;;; ESC F3
        "ENTER yankl"       =   esc esc [ 2 2 7 z   ;;; ESC F4
        "ENTER yankw"       =   esc esc [ 2 2 8 z   ;;; ESC F5
        "ENTER yankw"       =   esc esc [ 2 2 9 z   ;;; ESC F6
        "ENTER yankw"       =   esc esc [ 2 3 0 z   ;;; ESC F7
        ENTER mi            =   esc esc [ 2 3 1 z   ;;; ESC F8
        ENTER ti            =   esc esc [ 2 3 2 z   ;;; ESC F9
        "screenbell"        =   esc esc [ 2 3 3 z   ;;; F10 (type 4 kbd only)
    endvedset
#_ELSE
    vedset keys
        "screenbell"        =   esc [ 2 3 ~         ;;; L1 (F11)
        lineabove           =   esc [ 2 4 ~         ;;; L2 (F12)
        pushkey             =   esc [ 2 5 ~         ;;; L3
        linebelow           =   esc [ 2 6 ~         ;;; L4
        exchangeposition    =   esc [ 2 8 ~         ;;; L5
        "loadline"          =   esc [ 2 9 ~         ;;; L6
        popkey              =   esc [ 3 1 ~         ;;; L7
        "redokey"           =   esc [ 3 2 ~         ;;; L8
        statusswitch        =   esc [ 3 3 ~         ;;; L9
        "enterkey"          =   esc [ 3 4 ~         ;;; L10

        "screenbell"        =   esc esc [ 2 3 ~     ;;; ESC L1 (F11)
        "ENTER crm"         =   esc esc [ 2 4 ~     ;;; ESC L2 (F12)
        "ENTER break"       =   esc esc [ 2 5 ~     ;;; ESC L3
        "xrefresh"          =   esc esc [ 2 6 ~     ;;; ESC L4
        ENTER jp            =   esc esc [ 2 8 ~     ;;; ESC L5
        setstatic           =   esc esc [ 2 9 ~     ;;; ESC L6
        ENTER pop           =   esc esc [ 3 1 ~     ;;; ESC L7

        marklo              =   esc [ 1 1 ~         ;;; F1
        markhi              =   esc [ 1 2 ~         ;;; F2
        clearhead           =   esc [ 1 3 ~         ;;; F3
        linedelete          =   esc [ 1 4 ~         ;;; F4
        cleartail           =   esc [ 1 5 ~         ;;; F5
        wordleftdelete      =   esc [ 1 7 ~         ;;; F6
        wordrightdelete     =   esc [ 1 8 ~         ;;; F7
        ENTER m             =   esc [ 1 9 ~         ;;; F8
        ENTER t             =   esc [ 2 0 ~         ;;; F9
        "screenbell"        =   esc [ 2 1 ~         ;;; F10 (type 4 kbd only)

        ENTER mbf           =   esc esc [ 1 1 ~     ;;; ESC F1
        ENTER mef           =   esc esc [ 1 2 ~     ;;; ESC F2
        "ENTER yankw"       =   esc esc [ 1 3 ~     ;;; ESC F3
        "ENTER yankl"       =   esc esc [ 1 4 ~     ;;; ESC F4
        "ENTER yankw"       =   esc esc [ 1 5 ~     ;;; ESC F5
        "ENTER yankw"       =   esc esc [ 1 7 ~     ;;; ESC F6
        "ENTER yankw"       =   esc esc [ 1 8 ~     ;;; ESC F7
        ENTER mi            =   esc esc [ 1 9 ~     ;;; ESC F8
        ENTER ti            =   esc esc [ 2 0 ~     ;;; ESC F9
        "screenbell"        =   esc esc [ 2 1 ~     ;;; F10 (type 4 kbd only)
    endvedset
#_ENDIF
    vedset keys
        dotdelete           =   ^H                  ;;; Back Space

        textleft            =   esc [ 1 1 1 ~       ;;; R1
        screenup            =   esc [ 1 1 2 ~       ;;; R2
        textright           =   esc [ 1 1 3 ~       ;;; R3
        screenleft          =   esc [ 1 1 4 ~       ;;; R4
        screendown          =   esc [ 1 1 5 ~       ;;; R5
        screenright         =   esc [ 1 1 7 ~       ;;; R6
        charupleft          =   esc [ 1 1 8 ~       ;;; R7
        charup              =   esc [ A             ;;; R8
        charupright         =   esc [ 1 2 0 ~       ;;; R9
        charleft            =   esc [ D             ;;; R10
        "ENTER timed_esc"   =   esc [ 1 2 3 ~       ;;; R11
        charright           =   esc [ C             ;;; R12
        wordleft            =   esc [ 1 2 5 ~       ;;; R13
        chardown            =   esc [ B             ;;; R14
        wordright           =   esc [ 1 2 8 ~       ;;; R15

        "topwindow"         =   esc esc [ 1 1 1 ~   ;;; ESC R1
        topfile             =   esc esc [ 1 1 2 ~   ;;; ESC R2
        markfind            =   esc esc [ 1 1 3 ~   ;;; ESC R3
        "midwindow"         =   esc esc [ 1 1 4 ~   ;;; ESC R4
        endfile             =   esc esc [ 1 1 5 ~   ;;; ESC R5
        "endrange"          =   esc esc [ 1 1 7 ~   ;;; ESC R6
        charupleftlots      =   esc esc [ 1 1 8 ~   ;;; ESC R7
        charuplots          =   esc esc [ A         ;;; ESC R8
        charuprightlots     =   esc esc [ 1 2 0 ~   ;;; ESC R9
        charleftlots        =   esc esc [ D         ;;; ESC R10
        "midwindow"         =   esc esc [ 1 2 3 ~   ;;; ESC R11
        charrightlots       =   esc esc [ C         ;;; ESC R12
        chardownleftlots    =   esc esc [ 1 2 5 ~   ;;; ESC R13
        chardownlots        =   esc esc [ B         ;;; ESC R14
        chardownrightlots   =   esc esc [ 1 2 8 ~   ;;; ESC R15

    /*  Extra bindings for type 4 keyboard */

        "helpkey"           =   esc K               ;;; Help
        "ENTER hkey"        =   esc esc K           ;;; ESC Help
        wordleft            =   esc O p             ;;; KP 0
        wordright           =   esc O n             ;;; KP .
        "enterkey"          =   esc O M             ;;; Enter
        statusswitch        =   esc O k             ;;; KP +
        "redokey"           =   esc O m             ;;; KP -
        "screenbell"        =   esc [ 1 1 0 ~       ;;; NumLock

    /* Extra binding for type 5 keyboard
        (these are copied from LIB VEDNCDXTERMKEYS)
    */

        lineabove           =   esc [ 2 ~           ;;; INSERT
        "prevscreen"        =   esc [ 5 ~           ;;; PAGE UP
        "nextscreen"        =   esc [ 6 ~           ;;; PAGE DOWN
        topfile             =   esc <               ;;; HOME
        endfile             =   esc >               ;;; END

        linebelow           =   esc esc [ 2 ~       ;;; ESC INSERT
        ENTER xup           =   esc esc [ 5 ~       ;;; ESC PAGE UP
        ENTER xdn           =   esc esc [ 6 ~       ;;; ESC PAGE DOWN
        markfind            =   esc esc <           ;;; ESC HOME
        endrange            =   esc esc >           ;;; ESC END

    endvedset;

    'sunxterm' -> vedkeymapname;
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jul  6 1993
        Changed vedkeymapname to 'sunxterm'
--- John Williams, Apr 20 1993
        Added new key mappings for Sun type 5 keyboard
--- John Williams, Aug 17 1992
        Added ~ to the end of the character sequence for the NumLock key
--- John Williams, Oct 16 1990
        Merged with LIB * VEDSUNSFXTERMKEYS, added type 4 keyboard bindings,
        sets -vedkeymapname-.
--- Rob Duncan, Oct  9 1989
        Sectionised
--- Andreas Schoter, Aug 1 1989
        Modified to use vedset syntax and screen stuff seperated out
 */
