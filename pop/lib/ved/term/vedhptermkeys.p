/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedhptermkeys.p
 > Purpose:         VED customisation for HP VUE Terminal Window (UK keyboard)
 > Author:          Julian Clinton, October 1991 (see revisions)
 > Documentation:   HELP * HPXVEDKEYS
 > Related Files:   LIB *VEDXVEDKEYS
 */

compile_mode :pop11 +strict;

section;

define global vedhptermkeys();

    vedset keys

        ;;; Function keys

        charup          = esc A           ;;; up arrow
        chardown        = esc B           ;;; down arrow
        charleft        = esc D           ;;; left arrow
        charright       = esc C           ;;; right arrow

        screenup        = esc V           ;;; PREV
        screendown      = esc U           ;;; NEXT

        ;;; ESC plus function keys

        refresh         = esc esc p ^M      ;;; ESC (F1)
        ENTER yankw     = esc esc q ^M      ;;; ESC (F2)
        ENTER yankl     = esc esc r ^M      ;;; ESC (F3)
        ENTER yankw     = esc esc s ^M      ;;; ESC (F4)

        markfind        = esc esc t ^M      ;;; ESC (F5)
        "endrange"      = esc esc u ^M      ;;; ESC (F6)
        ENTER mi        = esc esc v ^M      ;;; ESC (F7)
        ENTER ti        = esc esc w ^M      ;;; ESC (F8)


    ;;; ESC + named keys

        screenup        = esc esc A           ;;; ESC up arrow
        screendown      = esc esc B           ;;; ESC down arrow
        screenleft      = esc esc D           ;;; ESC left arrow
        textright       = esc esc C           ;;; ESC right arrow

    ;;; general
        chardelete      = ^H                ;;; make backspace be delete
    endvedset;

    'hpterm' -> vedkeymapname;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Dec  3 1991
        Removed call to -ved*ansikeys-
 */
