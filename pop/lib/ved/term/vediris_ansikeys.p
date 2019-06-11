/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vediris_ansikeys.p
 > Purpose:         VED: key bindings for SGI Iris terminal emulator.
 > Author:          Robert Duncan, Dec 1991 (see revisions)
 > Documentation:   HELP * IRIS_ANSIKEYS
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

uses vedvt100keys;

define vediris_ansikeys();
    false -> vednokeypad;
    vedset screen
        setpad              = esc =
        resetpad            = esc >
    endvedset;
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

        clearhead           = esc [ 0 0 1 q         ;;; F1
        linedelete          = esc [ 0 0 2 q         ;;; F2
        cleartail           = esc [ 0 0 3 q         ;;; F3

        wordleftdelete      = esc [ 0 0 5 q         ;;; F5
        wordrightdelete     = esc [ 0 0 6 q         ;;; F6
        marklo              = esc [ 0 0 7 q         ;;; F7
        markhi              = esc [ 0 0 8 q         ;;; F8

        ENTER m             = esc O P               ;;; F9
        ENTER t             = esc O Q               ;;; F10
        pushkey             = esc O R               ;;; F11
        dotdelete           = esc O S               ;;; F12

        "ENTER yankw"       = esc esc [ 0 0 1 q     ;;; ESC F1
        "ENTER yankl"       = esc esc [ 0 0 2 q     ;;; ESC F2
        "ENTER yankw"       = esc esc [ 0 0 3 q     ;;; ESC F3

        "ENTER yankw"       = esc esc [ 0 0 5 q     ;;; ESC F5
        "ENTER yankw"       = esc esc [ 0 0 6 q     ;;; ESC F6
        ENTER mbf           = esc esc [ 0 0 7 q     ;;; ESC F7
        ENTER mef           = esc esc [ 0 0 8 q     ;;; ESC F8

        ENTER mi            = esc esc O P           ;;; ESC F9
        ENTER ti            = esc esc O Q           ;;; ESC F10
        popkey              = esc esc O R           ;;; ESC F11
        "refresh"           = esc esc O S           ;;; ESC F12

        lineabove           = esc [ 1 3 9 q         ;;; INSERT
        linebelow           = esc esc [ 1 3 9 q     ;;; ESC INSERT

        ENTER timed_esc     = esc O u               ;;; Keypad 5

    endvedset;
    'iris_ansi' -> vedkeymapname;
enddefine;

define vediris_ansi_netkeys();
    vediris_ansikeys();
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Dec  2 1991
        Modified to be more consistent with the XVed key bindings
 */
