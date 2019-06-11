/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedatari.p
 > Purpose:         Configure VED for Atari ST running UniTerm emulator
 > Author:          Leila Burrell-Davies, Dec  9 1988 (see revisions)
 > Documentation:   HELP * VEDATARI HELP * VEDATARIKEYS
 > Related Files:
 */
compile_mode :pop11 +strict;

;;; lib vedatari
;;; For Atari ST running UniTerm terminal emulator with VT102 emulation

;;; UniTerm automatically programs the numeric keypad and cursor keys
;;; exactly as VT100 series terminals. This library allows the 10
;;; function keys and Clr/Home key on the Atari to be used in addition.

;;; Will only work if UniTerm's setup file UNITERM.SET has the function keys
;;; F1 through F10 programmed as shown below, e.g.
;;;     F1 sends <ESC>f1
;;;     F2 sends <ESC>f2
;;;        etc. until
;;;     F10 sends <ESC>f0
;;; Similarly for shifted function keys
;;;     Shift and F1 sends <ESC>F1
;;;             etc.
;;; and Control and F1 sends <ESC>^F1
;;;        and so forth.
;;; (where ^F means ASCII character 6, i.e. Ctrl/F)
;;;
;;; Developed with UniTerm V2.0d, release 005. Some earlier versions
;;; do not allow Control and function key sequences to be defined.

uses vedvt100;

section;

define vars vedatari();
    ;;; It's like a vt100
    vedvt100(false);
    ;;; only better
    vedset keys
        topfile         = esc [ H           ;;; Clr/Home
        dotdelete       = esc f 1           ;;; F1
        clearhead       = esc f 2           ;;; F2
        linedelete      = esc f 3           ;;; F3
        cleartail       = esc f 4           ;;; F4
        wordleftdelete  = esc f 5           ;;; F5
        wordrightdelete = esc f 6           ;;; F6
        lineabove       = esc f 7           ;;; F7
        linebelow       = esc f 8           ;;; F8
        pushkey         = esc f 9           ;;; F9
        nextline        = esc f 0           ;;; F10

        "ENTER y"       = esc F 1           ;;; Shift/F1
        "ENTER yankw"   = esc F 2           ;;; Shift/F2
        "ENTER yankl"   = esc F 3           ;;; Shift/F3
        "ENTER yankw"   = esc F 4           ;;; Shift/F4
        "ENTER yankw"   = esc F 5           ;;; Shift/F5
        "ENTER yankw"   = esc F 6           ;;; Shift/F6
        ENTER xup       = esc F 7           ;;; Shift/F7
        ENTER xdn       = esc F 8           ;;; Shift/F8
        popkey          = esc F 9           ;;; Shift/F9
        setstatic       = esc F 0           ;;; Shift/F10

        "ENTER sw"      = esc ^F 1          ;;; Control/F1
        "ENTER ucl"     = esc ^F 2          ;;; Control/F2
        "ENTER lcl"     = esc ^F 3          ;;; Control/F3
        "ENTER ucw"     = esc ^F 4          ;;; Control/F4
        "ENTER lcw"     = esc ^F 5          ;;; Control/F5
        markfind        = esc ^F 6          ;;; Control/F6
        "endrange"      = esc ^F 7          ;;; Control/F7
        "ENTER jp"      = esc ^F 8          ;;; Control/F8
        "ENTER jjp"     = esc ^F 9          ;;; Control/F9
        "ENTER fill"    = esc ^F 0          ;;; Control/F10
    endvedset;
    'vedatari' -> vedkeymapname;
enddefine;

if iscaller(vedsetup) then vedatari() endif;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Oct 18 1990
        Assigned 'vedatari' to -vedkeymapname-.
--- Rob Duncan, Oct 18 1989
        Sectionised. Changed file status from local to system.
--- Andreas Schoter, Jul 12 1989
        re-writen to use vedset syntax
 */
