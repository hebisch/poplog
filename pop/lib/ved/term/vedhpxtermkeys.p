/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedhpxtermkeys.p
 > Purpose:         VED customisation for xterm on a HP 9000 (UK keyboard)
 > Author:          Rob Blackbourn, Roger Evans, May 7 1989 (see revisions)
 > Documentation:   HELP * VEDXTERM
 > Related Files:   LIB * VEDXTERMKEYS
 */
compile_mode :pop11 +strict;

section;

uses vedvt100keys;

define vedhpxtermkeys();
    vedvt100keys();

    vedset keys

    ;;; Function keys
        "xrefresh"      = esc [ 1 0 1 ~     ;;; BREAK
        setstatic       = esc [ 1 0 2 ~     ;;; STOP

        dotdelete       = esc [ 1 1 ~       ;;; F1
        clearhead       = esc [ 1 2 ~       ;;; F2
        linedelete      = esc [ 1 3 ~       ;;; F3
        cleartail       = esc [ 1 4 ~       ;;; F4

        wordleftdelete  = esc [ 2 9 ~       ;;; MENU
        wordrightdelete = esc [ 1 1 0 ~     ;;; USER

        marklo          = esc [ 1 5 ~       ;;; F5
        markhi          = esc [ 1 7 ~       ;;; F6
        ENTER m         = esc [ 1 8 ~       ;;; F7
        ENTER t         = esc [ 1 9 ~       ;;; F8


        loadline        = esc [ 1 0 5 ~     ;;; CLEAR LINE
        "ENTER jjp"     = ^K                ;;; CLEAR DISPLAY

        pushkey         = esc [ 2 0 ~
        popkey          = esc [ 2 1 ~
        topfile         = esc [ 2 3 ~
        endfile         = esc [ 2 4 ~

    ;;; ESC + function keys
        "xrefresh"      = esc esc [ 1 0 1 ~     ;;; ESC BREAK
        setstatic       = esc esc [ 1 0 2 ~     ;;; ESC STOP

        "ENTER sw"      = esc esc [ 1 1 ~       ;;; ESC F1
        "ENTER yankw"   = esc esc [ 1 2 ~       ;;; ESC F2
        "ENTER yankl"   = esc esc [ 1 3 ~       ;;; ESC F3
        "ENTER yankw"   = esc esc [ 1 4 ~       ;;; ESC F4

        "ENTER yankw"   = esc esc [ 2 9 ~       ;;; ESC MENU
        "ENTER yankw"   = esc esc [ 1 1 0 ~     ;;; ESC USER

        markfind        = esc esc [ 1 5 ~       ;;; ESC F5
        "endrange"      = esc esc [ 1 7 ~       ;;; ESC F6
        ENTER mi        = esc esc [ 1 8 ~       ;;; ESC F7
        ENTER ti        = esc esc [ 1 9 ~       ;;; ESC F8

        ENTER lmr       = esc esc [ 1 0 5 ~     ;;; ESC CLEAR LINE

    ;;; Other named keys
        ident enterkey  = esc [ 1 0 4 ~     ;;; PRINT
        lineabove       = esc [ 1 0 6 ~     ;;; INSERT LINE
        linebelow       = esc [ 1 0 7 ~     ;;; DELETE LINE
        setstatic       = esc [ 1 0 8 ~     ;;; INSERT CHAR
        dotdelete       = esc [ 1 0 9 ~     ;;; DELETE CHAR
        ident enterkey  = esc [ `           ;;; HOME
        fileselect      = esc [ 4 ~         ;;; SELECT
        "prevline"      = esc [ 5 ~         ;;; PREV
        nextline        = esc [ 6 ~         ;;; NEXT

    ;;; ESC + named keys
        switchstatus    = esc esc [ 1 0 4 ~     ;;; ESC PRINT
        linebelow       = esc esc [ 1 0 6 ~     ;;; ESC INSERT LINE
        "ENTER yankl"   = esc esc [ 1 0 7 ~     ;;; ESC DELETE LINE
        setstatic       = esc esc [ 1 0 8 ~     ;;; ESC INSERT CHAR
        ident screenbell= esc esc [ 1 0 9 ~     ;;; ESC DELETE CHAR
        statusswitch    = esc esc [ `           ;;; HOME
        ENTER files     = esc esc [ 4 ~         ;;; SELECT
        ENTER xup       = esc esc [ 5 ~         ;;; PREV
        ENTER xdn       = esc esc [ 6 ~         ;;; NEXT

    ;;; Arrow keys
        charup          = esc [ A
        chardown        = esc [ B
        charright       = esc [ C
        charleft        = esc [ D

    ;;; ESC + arrow keys
        charuplots      = esc esc [ A
        chardownlots    = esc esc [ B
        charrightlots   = esc esc [ C
        charleftlots    = esc esc [ D


    ;;; general
        chardelete      = ^H                ;;; make backspace be delete
        ENTER xup       = esc ^H            ;;; ESC BS
        ident redokey   = esc O M           ;;; swap ENTER and TAB
        ident enterkey  = esc O I
        textleft        = esc O j           ;;; *
        textright       = esc O o           ;;; /
        screendown      = esc O m           ;;; +
        screenup        = esc O k           ;;; -
    endvedset;

    'hpxterm' -> vedkeymapname;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan  5 1993
        Replaced quoted words with idents for some core variable ops
--- John Williams, Nov 16 1990
        Sets -vedkeymapname-
--- John Williams, Jun  6 1990
        Now runs -vedvt100keys- before defining HP xterm keys
--- Rob Duncan, Oct  9 1989
        Sectionised
--- Andreas Schoter, Aug  7 1989
        Modified to use vedset notation
*/
