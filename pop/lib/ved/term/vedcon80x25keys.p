/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedcon80x25keys.p
 > Purpose:         VED: key bindings for Linux PC console.
 > Author:          Julian Clinton, Sep 1995
 >                  (based on vedirisansi_keys.p and vedvt220keys.p)
 > Documentation:   HELP * CON80X25KEYS
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

uses vedvt100keys;

define vedcon80x25keys();
    false -> vednokeypad;
    vedset screen
        setpad              = esc =
        resetpad            = esc >
    endvedset;
    vedvt100keys();
    vedset keys
        charup              = esc [ A               ;;; UP
        chardown            = esc [ B               ;;; DOWN
        charleft            = esc [ D               ;;; LEFT
        charright           = esc [ C               ;;; RIGHT

        charuplots          = esc esc [ A           ;;; ESC UP
        chardownlots        = esc esc [ B           ;;; ESC DOWN
        charleftlots        = esc esc [ D           ;;; ESC LEFT
        charrightlots       = esc esc [ C           ;;; ESC RIGHT

        ENTER hkeys         = esc [ [ A             ;;; F1
        clearhead           = esc [ [ B             ;;; F2
        linedelete          = esc [ [ C             ;;; F3
        cleartail           = esc [ [ D             ;;; F4

        ENTER hk            = esc esc [ [ A         ;;; ESC F1
        ENTER yankw         = esc esc [ [ B         ;;; ESC F2
        ENTER yankl         = esc esc [ [ C         ;;; ESC F3
        ENTER yankw         = esc esc [ [ D         ;;; ESC F4

        wordleftdelete      = esc [ [ E             ;;; F5
        wordrightdelete     = esc [ 1 7 ~           ;;; F6
        marklo              = esc [ 1 8 ~           ;;; F7
        markhi              = esc [ 1 9 ~           ;;; F8

        ENTER yankw         = esc esc [ [ E         ;;; ESC F5
        ENTER yankw         = esc esc [ 1 7 ~       ;;; ESC F6
        ENTER mbf           = esc esc [ 1 8 ~       ;;; ESC F7
        ENTER mef           = esc esc [ 1 9 ~       ;;; ESC F8

        ENTER m             = esc [ 2 0 ~           ;;; F9
        ENTER t             = esc [ 2 1 ~           ;;; F10
        pushkey             = esc [ 2 3 ~           ;;; F11
        dotdelete           = esc [ 2 4 ~           ;;; F12

        ENTER mi            = esc esc [ 2 0 ~       ;;; ESC F9
        ENTER ti            = esc esc [ 2 1 ~       ;;; ESC F10
        popkey              = esc esc [ 2 3 ~       ;;; ESC F11
        refresh             = esc esc [ 2 4 ~       ;;; ESC F12

        dotdelete           = esc [ 3 ~             ;;; DELETE
        dotdelete           = esc esc [ 3 ~         ;;; ESC DELETE
        setstatic           = esc [ 2 ~             ;;; INSERT
        setstatic           = esc esc [ 2 ~         ;;; ESC INSERT
        screenleft          = esc [ 1 ~             ;;; HOME
        topfile             = esc esc [ 1 ~         ;;; ESC HOME
        textright           = esc [ 4 ~             ;;; END
        endfile             = esc esc [ 4 ~         ;;; ESC END
        prevscreen          = esc [ 5 ~             ;;; PG UP
        screenup            = esc esc [ 5 ~         ;;; ESC PG UP
        nextscreen          = esc [ 6 ~             ;;; PG DN
        screendown          = esc esc [ 6 ~         ;;; ESC PG DN

        redocommand         = esc O S               ;;; KP -
        switchstatus        = esc O L               ;;; KP +
        loadline            = esc O R               ;;; KP *
        ENTER re_search     = esc O Q               ;;; KP /
    endvedset;
    'con80x25' -> vedkeymapname;
enddefine;

endsection;
