/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvt220keys.p
 > Purpose:         Configure VED for VT220 type terminals
 > Author:          John Williams, Oct 13 1989 (see revisions)
 > Documentation:   HELP * VT220KEYS
 > Related Files:
 */
compile_mode :pop11 +strict;

uses vedvt100keys;

section;

define vedvt220keys();

    vedvt100keys();

    /* Make "Select" key an escape key (because DEC VT220 has no esc) */

    vedsetkey('\^[[4~', '\^[');

    vedset keys

        /* Make keypad 5 a timed escape key */

        ENTER timed_esc     = esc O u

        /* Arrow keys */

        screenup            = esc [ A
        screendown          = esc [ B
        textright           = esc [ C
        screenleft          = esc [ D

        topfile             = esc esc [ A
        endfile             = esc esc [ B
        screenright         = esc esc [ C
        textleft            = esc esc [ D

        /* Function keys */

        clearhead           = esc [ 1 7 ~       ;;; F6
        linedelete          = esc [ 1 8 ~       ;;; F7
        cleartail           = esc [ 1 9 ~       ;;; F8
        wordleftdelete      = esc [ 2 0 ~       ;;; F9
        wordrightdelete     = esc [ 2 1 ~       ;;; F10

        "ENTER yankw"       = esc esc [ 1 7 ~   ;;; ESC F6
        "ENTER yankl"       = esc esc [ 1 8 ~   ;;; ESC F7
        "ENTER yankw"       = esc esc [ 1 9 ~   ;;; ESC F8
        "ENTER yankw"       = esc esc [ 2 0 ~   ;;; ESC F9
        "ENTER yankw"       = esc esc [ 2 1 ~   ;;; ESC F10

        marklo              = esc [ 2 3 ~       ;;; F11
        markhi              = esc [ 2 4 ~       ;;; F12
        ENTER m             = esc [ 2 5 ~       ;;; F13
        ENTER t             = esc [ 2 6 ~       ;;; F14

        ENTER mbf           = esc esc [ 2 3 ~   ;;; ESC F11
        ENTER mef           = esc esc [ 2 4 ~   ;;; ESC F12
        ENTER mi            = esc esc [ 2 5 ~   ;;; ESC F13
        ENTER ti            = esc esc [ 2 6 ~   ;;; ESC F14

        pushkey             = esc [ 3 1 ~       ;;; F17
        exchangeposition    = esc [ 3 2 ~       ;;; F18
        setstatic           = esc [ 3 3 ~       ;;; F19
        dotdelete           = esc [ 3 4 ~       ;;; F20

        popkey              = esc esc [ 3 1 ~   ;;; ESC F17
        ENTER cps           = esc esc [ 3 2 ~   ;;; ESC F18
        "ENTER break"       = esc esc [ 3 3 ~   ;;; ESC F19
        refresh             = esc esc [ 3 4 ~   ;;; ESC F20

        /* Labelled keys (NB Select = Escape (see above)) */

        "helpkey"           = esc [ 2 8 ~       ;;; Help
        "loadline"          = esc [ 2 9 ~       ;;; Do

        "ENTER hkey"        = esc esc [ 2 8 ~   ;;; ESC Help
        ENTER lmr           = esc esc [ 2 9 ~   ;;; ESC Do

        markfind            = esc [ 1 ~         ;;; Find
        lineabove           = esc [ 2 ~         ;;; Insert Here
        ENTER d             = esc [ 3 ~         ;;; Remove
        "prevscreen"        = esc [ 5 ~         ;;; Prev Screen
        "nextscreen"        = esc [ 6 ~         ;;; Next Screen

        "endrange"          = esc esc [ 1 ~     ;;; ESC Find
        linebelow           = esc esc [ 2 ~     ;;; ESC Insert Here
        "ENTER y"           = esc esc [ 3 ~     ;;; ESC Remove
        "screenbell"        = esc esc [ 4 ~     ;;; ESC Select (= ESC ESC)
        ENTER xup           = esc esc [ 5 ~     ;;; ESC Prev Screen
        ENTER xdn           = esc esc [ 6 ~     ;;; ESC Next Screen

    endvedset;
    'vt220' -> vedkeymapname;
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Nov 20 1990
        Changed "vedscreenbell" to "screenbell" (BR guest4@cogs.susx.ac.uk.1)
--- John Williams, Oct 12 1990
        -vedkeymapname- instead of -vedkeyboardname-
--- John Williams, Oct  5 1990
        Now uses -vedhelpkey-, and sets -vedkeyboardname-
--- Rob Duncan, Oct 24 1989
        Moved out of "vedvt220.p" and adapted to use vedset notation.
 */
