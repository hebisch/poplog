/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedncdxtermkeys.p
 > Purpose:         VED: key bindings for NCD X terminal
 > Author:          Rob Smith, Sep 14 1989 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

/* This file defines the procedure -vedncdxtermkeys- which customises
    VED's key bindings for the keyboard of an NCD16 X terminal

    In order to use the keys marked "Home" and "End" in VED, the following
    key translations must be added to /usr/lib/X11/app-defaults/XTerm:

        XTerm*VT100*Translations: #override \
            <Key>Home: string(0x1b) string("<") \n\
            <Key>End: string(0x1b) string(">")

    i.e. HOME will send ESC <, and END will send ESC >
*/

section;

uses vedvt100keys;

define vedncdxtermkeys();
    vedvt100keys();
    vedset keys
        screenup            = esc [ A               ;;; UP
        screendown          = esc [ B               ;;; DOWN
        screenleft          = esc [ D               ;;; LEFT
        textright           = esc [ C               ;;; RIGHT

        topfile             = esc esc [ A           ;;; ESC UP
        endfile             = esc esc [ B           ;;; ESC DOWN
        textleft            = esc esc [ D           ;;; ESC LEFT
        screenright         = esc esc [ C           ;;; ESC RIGHT

        dotdelete           = esc [ 1 1 ~           ;;; F1
        clearhead           = esc [ 1 2 ~           ;;; F2
        linedelete          = esc [ 1 3 ~           ;;; F3
        cleartail           = esc [ 1 4 ~           ;;; F4

        wordleftdelete      = esc [ 1 5 ~           ;;; F5
        wordrightdelete     = esc [ 1 7 ~           ;;; F6
        marklo              = esc [ 1 8 ~           ;;; F7
        markhi              = esc [ 1 9 ~           ;;; F8

        ENTER m             = esc [ 2 0 ~           ;;; F9
        ENTER t             = esc [ 2 1 ~           ;;; F10
        pushkey             = esc [ 2 3 ~           ;;; F11
        exchangeposition    = esc [ 2 4 ~           ;;; F12

        "xrefresh"          = esc esc [ 1 1 ~       ;;; ESC F1
        "ENTER yankw"       = esc esc [ 1 2 ~       ;;; ESC F2
        "ENTER yankl"       = esc esc [ 1 3 ~       ;;; ESC F3
        "ENTER yankw"       = esc esc [ 1 4 ~       ;;; ESC F4

        "ENTER yankw"       = esc esc [ 1 5 ~       ;;; ESC F5
        "ENTER yankw"       = esc esc [ 1 7 ~       ;;; ESC F6
        ENTER mbf           = esc esc [ 1 8 ~       ;;; ESC F7
        ENTER mef           = esc esc [ 1 9 ~       ;;; ESC F8

        ENTER mi            = esc esc [ 2 0 ~       ;;; ESC F9
        ENTER ti            = esc esc [ 2 1 ~       ;;; ESC F10
        popkey              = esc esc [ 2 3 ~       ;;; ESC F11
        ENTER cps           = esc esc [ 2 4 ~       ;;; ESC F12

        lineabove           = esc [ 2 ~             ;;; INSERT
        "prevscreen"        = esc [ 5 ~             ;;; PAGE UP
        "nextscreen"        = esc [ 6 ~             ;;; PAGE DOWN
        topfile             = esc <                 ;;; HOME
        endfile             = esc >                 ;;; END

        linebelow           = esc esc [ 2 ~         ;;; ESC INSERT
        ENTER xup           = esc esc [ 5 ~         ;;; ESC PAGE UP
        ENTER xdn           = esc esc [ 6 ~         ;;; ESC PAGE DOWN
        markfind            = esc esc <             ;;; ESC HOME
        endrange            = esc esc >             ;;; ESC END

        ENTER timed_esc     = esc O u               ;;; Keypad 5
        "helpkey"           = esc O o               ;;; Keypad /
        "loadline"          = esc O j               ;;; Keypad *
        statusswitch        = esc O k               ;;; Keypad +

        "ENTER hkey"        = esc esc O o           ;;; ESC Keypad /
        ENTER lmr           = esc esc O j           ;;; ESC Keypad *

    endvedset;
    'ncdxterm' -> vedkeymapname;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 18 1992
        Added key mappings for the HOME and END keys
--- John Williams, Oct 12 1990
        -vedkeymapname- instead of -vedkeyboardname-
--- John Williams, Oct  5 1990
        Now uses -vedhelpkey-, and sets -vedkeyboardname-
--- John Williams & Rob Duncan, Nov  8 1989
        Function key definitions changed to be more similar to
        LIB VEDVT220KEYS
--- Rob Duncan, Oct  9 1989
        Sectionised; added call to -vedvt100keys-
--- Andreas Schoter Sep 1989
        Modified to use the -vedset- notation
 */
