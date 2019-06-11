/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/term/vedtvikeys.p
 >  Purpose:        For TELEVIDEO MODEL TVI 920C Emulating VT52
 >  Author:         A Sloman, 1982 (see revisions)
 >  Documentation:  HELP * TVI
 >  Related Files:  LIB * TVI925
 */
compile_mode :pop11 +strict;

;;; This caused real problems because the terminals don't correspond to
;;; the manual. I assume this is because of VT52 emulation.

;;; The following assumes twelve function keys, F1 to F12, which transmit
;;;     ESC ? CHAR.

;;; Then there are 4 more keys marked
;;;         Char Insert, Char delete, Line insert, Line Delete.

;;; Above the keypad are three more keys, one simulating GOLD (ESC P) and the
;;; other two duplicating Char Insert, and Line Delete. !!!

;;; The four Arrow function keys work as on the Visual 200, and are left for
;;; large moves up, down, left, right, as with the Visual 200 version.

;;; The general plan is as follows
;;; Keys F1 to F11 work exactly as on V200.
;;; F12 does VEDEXCHANGEPOSITION (ESC P on V200)

;;; CHAR INSERT key does insert line above
;;; CHAR DELETE key deletes char under cursor
;;; LINE INSERT key inserts line below
;;; LINE DELETE key does line delete (as does F2)
;;; The key with blue dot, above keypad 7 does screen refresh
;;; The next two keys duplicate the CHAR INSERT and LINE DELETE KEYS.

;;; Holding the FUNCT button down changes all keys so that they transmit
;;; CTRL-A, then the normal character (controlled by shift) then CR
;;; This means that CTRL-A can no longer be used for VEDSCREENLEFT
;;; However, VEDREADFUNCTIONKEY has been defined so that three CTRL-A's in
;;; succession will do screen left!

;;; The FUNC key has been used to modify the keypad, so that they numbers
;;; 1 to 9 with FUNC key depressed work exactly as on V200.
;;; For bigger jumps do ESC then key. E.g. ESC 9 does VEDCHARUPLOTS.

;;; The Comma and Dot, with FUNC do word left or word right.
;;; Instead of ENTER it is necessary to press FUNC and RETURN,
;;;     or FUNC and ENTER

;;; FUNC a does go to top of file
;;; FUNC z does go to end of file
;;; FUNC g does REDO last command.

;;;         ************************************

;;; a few VED procedures need to be re-defined.


section;

define lconstant vedreadfunctionkey();
    ;;; when the FUNCTION button is depressed, any other key causes
    ;;; transmission of CTRL-A, then the normal code, then RETURN
    ;;; This procedure is invoked when CTRL-A is read in
    lvars vedchar;
    vedinascii() -> vedchar;    ;;; Read the character
    erase(vedinascii());    ;;; read carriage return, but ignore it

    switchon vedchar ==
    case `7  then vedcharupleft()    ;;; Use keypad for moves
    case `8  then vedcharup()
    case `9  then vedcharupright()
    case `4  then vedcharleft()
    case `5  then vedcharmiddle()
    case `6  then vedcharright()
    case `1  then vedchardownleft()
    case `2  then vedchardown()
    case `3  then vedchardownright()
    case `0  then vedstatusswitch()
    case `,  then vedwordleft()
    case `.  then vedwordright()
    case `\^A` then vedscreenleft()     ;;; CTRL-A typed 3 times will do this!
    case `a then vedtopfile();
    case `z then vedendfile();
    case `g then vedredokey();        ;;; redo last command
    case `\^M then vedenterkey();          ;;; treat return key as ENTER
    case `- then vedredokey();
    else vedscreenbell()
    endswitchon
enddefine;


define vedtvikeys();

    vedset keys
        charupleftlots      = esc 7
        charuplots          = esc 8
        charuprightlots     = esc 9
        charleftlots        = esc 4
        charrightlots       = esc 6
        chardownleftlots    = esc 1
        chardownlots        = esc 2
        chardownrightlots   = esc 3
        textleft            = esc ,
        lineabove           = esc Q         ;;; char insert key
        linebelow           = esc E         ;;; line insert key
        screenright         = esc .
        dotdelete           = esc W         ;;; char delete key
        wordleftdelete      = esc ? s       ;;;F4
        wordrightdelete     = esc ? t       ;;;F5
        clearhead           = esc ? p       ;;;F1
        cleartail           = esc ? r       ;;;F3
        linedelete          = esc ? q       ;;;F2
        linedelete          = esc R         ;;; delete line key
        pushkey             = esc ? y       ;;;F10
        popkey              = esc ? n       ;;;F11
        readfunctionkey     = ctrl A        ;;; initiate function key sequence
        ENTER m             = esc ? x       ;;;F9
        marklo              = esc ? v       ;;;F7
        markhi              = esc ? w       ;;;F8
        statusswitch        = esc cr        ;;; ESC RETURN, go back to command line
        setstatic           = esc ? u       ;;;F6
        exchangeposition    = esc ? M       ;;;F12
        charmiddle          = esc 5
        refresh             = esc P         ;;; Key above the 7 on keypad (GOLD)
    endvedset

enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Jason Handby, Jul 12 1988 - split into separate files and changed to use
    vedset notation, changed so procedure is no longer added on the end of
    -vedinit-
--- Ben Rubinstein, Oct 12 1986 - vedenter, vedredo indirected through ..key
*/
