/* --- Copyright University of Sussex 1990.  All rights reserved. ---------
 > File:            C.all/lib/ved/term/vedvi55keys.p
 > Purpose:         VED: Key bindings for Visual 55
 > Author:          Aaron Sloman and Chris Slymon (see revisions)
 > Documentation:   HELP * V55
 > Related Files:   LIB * SETV55KEYS
 */
compile_mode :pop11 +strict;

uses vedvi200keys;

section;

;;; ved_v55:
;;;     program function keys.
;;;     Kept for backward compatability.

define vars ved_v55();
    ;;; program the function keys to send sequences: ESC ? <char>
    ;;; where <char> is a control character: A to M
    vedoutascii('\^[@1\^[?A\^[|\^[@2\^[?B\^[|\^[@3\^[?C\^[|\^[@4\^[?D\^[|');
    vedoutascii('\^[@5\^[?E\^[|\^[@6\^[?F\^[|\^[@7\^[?G\^[|\^[@8\^[?H\^[|');
    vedoutascii('\^[@9\^[?I\^[|\^[@A\^[?J\^[|\^[@B\^[?K\^[|\^[@C\^[?L\^[|');
    sysflush(poprawdevout)
enddefine;


define vedvi55keys();

    vedvi200keys();
    ved_v55();

    vedset keys;
    ;;; keypad keys
        ENTER timed_esc     = esc ? u       ;;; 5
        charupleft          = esc ? w       ;;; 7
        charup              = esc ? x       ;;; 8
        charupright         = esc ? y       ;;; 9
        charleft            = esc ? t       ;;; 4
        charright           = esc ? v       ;;; 6
        chardownleft        = esc ? q       ;;; 1
        chardown            = esc ? r       ;;; 2
        chardownright       = esc ? s       ;;; 3
        charmiddle          = esc esc ? u   ;;; ESC 5
        charupleftlots      = esc esc ? w   ;;; ESC 7
        charuplots          = esc esc ? x   ;;; ESC 8
        charuprightlots     = esc esc ? y   ;;; ESC 9
        charleftlots        = esc esc ? t   ;;; ESC 4
        charrightlots       = esc esc ? v   ;;; ESC 6
        chardownleftlots    = esc esc ? q   ;;; ESC 1
        chardownlots        = esc esc ? r   ;;; ESC 2
        chardownrightlots   = esc esc ? s   ;;; ESC 3
        charuplots <> "midwindow"
                            = esc H esc ? x ;;; HOME 8
        "midwindow"         = esc H esc ? u ;;; HOME 5
        chardownlots <> "midwindow"
                            = esc H esc ? r ;;; HOME 2

        charupleftlots      = esc i     ;;; function + pad 7
        charuplots          = esc K     ;;; function + pad 5
        charuprightlots     = esc O     ;;; function + pad 9
        charleftlots        = esc f     ;;; function + pad 4
        charrightlots       = esc J     ;;; function + pad 6
        chardownleftlots    = esc L     ;;; function + pad 1
        chardownlots        = esc v     ;;; function + pad 2
        chardownrightlots   = esc M     ;;; function + pad 3

        wordleft            = esc ? p       ;;; keypad 0
        wordright           = esc ? n       ;;; keypad .
        wordleftdelete      = esc esc ? p   ;;; ESC keypad 0
        wordrightdelete     = esc esc ? n   ;;; ESC keypad .
        "redokey"           = esc ? m       ;;; keypad -
        "enterkey"          = esc ? M       ;;; ENTER
        statusswitch        = esc ? l       ;;; keypad ,
        marklo              = esc esc ? m   ;;; ESC keypad -
        markhi              = esc esc ? l   ;;; ESC keypad ,

    ;;; function keys
        dotdelete           = esc ? A       ;;; F1
        clearhead           = esc ? B       ;;; F2
        linedelete          = esc ? C       ;;; F3
        cleartail           = esc ? D       ;;; F4
        wordleftdelete      = esc ? E       ;;; F5
        wordrightdelete     = esc ? F       ;;; F6
        marklo              = esc ? G       ;;; F7
        markhi              = esc ? H       ;;; F8
        ENTER m             = esc ? I       ;;; F9
        pushkey             = esc ? J       ;;; F10
        "ENTER jp"          = esc ? K       ;;; F11
        lineabove           = esc ? L       ;;; F12
        "ENTER yankw"       = esc esc ? B   ;;; ESC F2
        "ENTER yankl"       = esc esc ? C   ;;; ESC F3
        "ENTER yankw"       = esc esc ? D   ;;; ESC F4
        "ENTER yankw"       = esc esc ? E   ;;; ESC F5
        "ENTER yankw"       = esc esc ? F   ;;; ESC F6
        "ENTER ucl"         = esc R         ;;; FUNCTION F3
        "ENTER lcl"         = esc 4         ;;; FUNCTION F4
        "ENTER ucw"         = esc 5         ;;; FUNCTION F5
        "ENTER lcw"         = esc 6         ;;; FUNCTION F6
        linedelete <> "ENTER yankl" <> "ENTER yankl"
                            = esc 9         ;;; FUNCTION F9
        setstatic           = esc <         ;;; FUNCTION F12
        ENTER ti            = esc G         ;;; SHIFT F7
        exchangeposition    = esc j         ;;; SHIFT F10
        "ENTER jjp"         = esc k         ;;; SHIFT F11
        ENTER rb            = esc l         ;;; SHIFT F12
        "ENTER sw"          = ctrl B A      ;;; CTRL-F1
        "ENTER yankw"       = ctrl B B      ;;; CTRL-F2
        "ENTER yankl"       = ctrl B C      ;;; CTRL-F3
        "ENTER yankw"       = ctrl B D      ;;; CTRL-F4
        "ENTER yankw"       = ctrl B E      ;;; CTRL-F5
        "ENTER yankw"       = ctrl B F      ;;; CTRL-F6
        markfind            = ctrl B G      ;;; CTRL-F7
        "endrange"          = ctrl B H      ;;; CTRL-F8
        ENTER t             = ctrl B I      ;;; CTRL-F9
        popkey              = ctrl B J      ;;; CTRL-F10
        "ENTER fill"        = ctrl B K      ;;; CTRL-F11
        linebelow           = ctrl B L      ;;; CTRL-F12
        ENTER v55           = ctrl B Q      ;;; CTRL-SHIFT-F1
        ENTER clear         = ctrl B S      ;;; CTRL-SHIFT-F3
        "ENTER deof"        = ctrl B T      ;;; CTRL-SHIFT-F4
        "ENTER swl"         = ctrl B U      ;;; CTRL-SHIFT-F5
        "ENTER swr"         = ctrl B V      ;;; CTRL-SHIFT-F6
        ENTER to            = ctrl B W      ;;; CTRL-SHIFT-F7
        ENTER mo            = ctrl B X      ;;; CTRL-SHIFT-F8
        ENTER mi            = ctrl B Y      ;;; CTRL-SHIFT-F9
        ENTER cps           = ctrl B Z      ;;; CTRL-SHIFT-F10
        "ENTER tidy"        = ctrl B [      ;;; CTRL-SHIFT-F11

    ;;; arrow keys
        screenup            = esc A         ;;; up arrow
        screendown          = esc B         ;;; down arrow
        textright           = esc C         ;;; right arrow
        screenleft          = esc D         ;;; left arrow
        charup <> ENTER m   = esc esc A     ;;; ESC up-arrow (MOVE above)
        ENTER m             = esc esc B     ;;; ESC down-arrow (MOVE below)
        linebelow           = esc esc C     ;;; ESC right arrow
        lineabove           = esc esc D     ;;; ESC left arrow
        topfile             = esc H esc A   ;;; HOME uparrow
        endfile             = esc H esc B   ;;; HOME downarrow
        screenright         = esc H esc C   ;;; HOME right arrow
        textleft            = esc H esc D   ;;; HOME left arrow
        refresh             = esc H esc H   ;;; HOME HOME

    ;;; misc.
        wordleft            = ctrl B ctrl B
    endvedset

enddefine;


endsection;


/*  --- Revision History ---------------------------------------------------
--- Aaron Sloman, May 11 1990
        Restored missing function+keypad options, and restored tabs.
--- Andreas Schoter, April 4 1990
        Fixed some incorrect keybindings, and tidied it up a bit
--- Jason Handby, Jul 12 1988
        Split into two files, changed to use vedset notation,
        changed "v55" to "vi55"
*/
