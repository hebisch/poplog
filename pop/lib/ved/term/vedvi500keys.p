/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/term/vedvi500keys.p
 >  Purpose:        Conversion of VED for Visual 500 with VT52 emulation.
 >  Author:         A Sloman, Jan 1984 (see revisions)
 >  Documentation:  HELP * V500
 >  Related Files:
 */
compile_mode :pop11 +strict;

uses vedvi200keys;

section;

define vedvi500keys();

    vedvi200keys();

    vedset keys
        wordleft            = esc ? p    ;;; keypad 0
        wordright           = esc ? n    ;;; keypad .
        charupleftlots      = esc i      ;;; 7
        charupleftlots      = esc esc ? w           ;;; 7
        charuplots          = esc K      ;;; 5
        charuplots          = esc esc ? x           ;;; 8
        charuprightlots     = esc O      ;;; 9
        charuprightlots     = esc esc ? y           ;;; 9
        charleftlots        = esc f      ;;; 4
        charleftlots        = esc esc ? t           ;;; 4
        charrightlots       = esc J      ;;; 6
        charrightlots       = esc esc ? v           ;;; 6
        chardownleftlots    = esc L      ;;; 1
        chardownleftlots    = esc esc ? q           ;;; 1 on keypad
        chardownlots        = esc esc ? r           ;;; 2
        chardownrightlots   = esc M      ;;; 3
        chardownrightlots   = esc esc ? s           ;;; 3
        lineabove           = esc esc D
        linebelow           = esc esc C
        topfile             = esc esc H
        dotdelete           = esc ctrl ?
        dotdelete           = esc P
        wordleftdelete      = esc esc ? p   ;;; ESC keypad 0
        wordleftdelete      = esc T
        wordrightdelete     = esc esc ? n   ;;; ESC keypad .
        wordrightdelete     = esc U
        clearhead           = esc Q
        cleartail           = esc S
        linedelete          = esc R
        "redokey"           = esc ? m    ;;; keypad -
        pushkey             = esc Z
        popkey              = esc [
        ENTER m             = esc Y
        marklo              = esc esc ? m   ;;; ESC keypad -
        markhi              = esc esc ? l   ;;; ESC keypad ,
        marklo              = esc W
        markhi              = esc X
        switchstatus        = esc ? l    ;;; keypad ,
        refresh             = esc esc ? M   ;;; ESC ENTER
        midwindow           = esc esc ? u           ;;; 5
        setstatic           = esc V
    endvedset

enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- Jason Handby, Jul 21 1989
    Split into seperate files, altered to use vedset notation
--- Poplog System, Nov 29 1988 - Added "uses graphic" (cf ALPHA 8)
--- Ben Rubinstein, Oct 12 1986 - vedenter, vedredo indirected through ..key
*/
