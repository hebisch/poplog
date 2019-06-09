/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.x/x/ved/auto/vedhpxvedkeys.p
 > Purpose:         VED customisation for xved on a HP 9000 (UK keyboard)
 > Author:          Rob Blackbourn, Roger Evans, May 7 1989 (see revisions)
 > Documentation:   HELP * HPXVEDKEYS
 > Related Files:   LIB *VEDXVEDKEYS
 */
compile_mode :pop11 +strict;

section;

define vedhpxvedkeys();
    vedset keys

    ;;; Function keys
        refresh         = (Break)      ;;; BREAK
        setstatic       = (Cancel)    ;;; STOP

        dotdelete       = (F1)

        wordleftdelete  = (Menu)                    ;;; MENU
        wordrightdelete = (keysym:1000FF6D)     ;;; System

        marklo          = (F5)

        markhi          = (F6)
        ENTER m         = (F7)
        ENTER t         = (F8)

        "ENTER jjp"     = (Clear)                 ;;; CLEAR DISPLAY
        loadline        = (keysym:1000FF6F)   ;;; Clear Line

        pushkey         = (F9)
        popkey          = (F10)
        topfile         = (F11)
        endfile         = (F12)

        ;;; Escape plus keys
        refresh        = esc (Break)
        setstatic      = esc (Cancel)           ;;; Stop

        "ENTER yankw"   = esc (Menu)
        "ENTER yankw"   = esc (keysym:1000FF6D)

        markfind        = esc (F5)
        "endrange"      = esc (F6)
        ENTER mi        = esc (F7)
        ENTER ti        = esc (F8)

        ENTER lmr       = esc (keysym:1000FF6F)
        "ENTER jjp"     = esc (Clear)

    ;;; Other named keys
        "enterkey"      = (Execute)            ;;; Print/Enter
        lineabove       = (keysym:1000FF70) ;;; INSERT LINE
        linebelow       = (keysym:1000FF71) ;;;  DELETE LINE
        setstatic       = (keysym:1000FF72) ;;; INSERT CHAR
        dotdelete       = (keysym:1000FF73) ;;; DELETE CHAR
        "enterkey"      = (Home)
        fileselect      = (Select)

    ;;; ESC + named keys
        switchstatus    = esc (Execute)             ;;; ESC PRINT
        linebelow       = esc (keysym:1000FF70)  ;;; ESC INSERT LINE
        "ENTER yankl"   = esc (keysym:1000FF71)  ;;; ESC DELETE LINE
        setstatic       = esc (keysym:1000FF72)  ;;; ESC INSERT CHAR
        "screenbell"    = esc (keysym:1000FF73)  ;;; ESC DELETE CHAR
        statusswitch    = esc (Home)
        ENTER files     = esc (Select)

        ENTER xup       = esc (Prior)           ;;; PREV
        ENTER xdn       = esc (Next)            ;;; NEXT

    ;;; general
        chardelete      = ^H                ;;; make backspace be delete
        ENTER xup       = esc ^H            ;;; ESC BS
        "redokey"       = (KP_Enter)        ;;; swap ENTER and TAB
        "enterkey"      = (KP_Tab)
        statusswitch    = (KP_Separator)

        textleft        = (KP_Multiply)     ;;; *
        textright       = (KP_Divide)       ;;; /
        screendown      = (KP_Add)          ;;; +
        screenup        = (KP_Subtract)     ;;; -
    endvedset;

    'hpxved' -> vedkeymapname;
enddefine;
;;;
uses-by_name (vedhpxvedkeys);

endsection;


/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jul 31 1991
        Made it use compile mode strict
*/
