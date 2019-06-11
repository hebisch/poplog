/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/term/vedtvi925keys.p
 >  Purpose:        FOR TELEVIDEO VDU MODEL TVI 925 (Non emulating)
 >  Author:         Jim Hunter, April 1983 (see revisions)
 >  Documentation:  HELP * VED, VEDKEYS
 >  Related Files:  LIB *TVI *TVI925 *TVI925SCREEN
 */
compile_mode :pop11 +strict;

/*
This package is provided for user convenience. It is not supported,
and it may change.

The following assumes eleven function keys, F1 to F11, which transmit
    CTRL-A CHAR CR

Then there are 6 more keys:

        KEY             : FUNCTION
        Char Insert     : Line Insert Above
        Char Delete     : Delete Char
        Line Insert     : Line Insert Below
        Line Delete     : Line Delete
        Line Erase      : Refresh Screen
        Page Erase      : Enter Command

The four Arrow function keys produce small moves in the appropriate
direction. When preceeded by ESC they act as they would on the Visual 200
without the ESC (i.e. large moves).

The general plan is as follows

Keys F1 to F11 work exactly as on V200.

CHAR INSERT key does insert line above
CHAR DELETE key deletes char under cursor
LINE INSERT key inserts line below
LINE DELETE key does line delete (as does F2)

Holding the FUNCT button down changes all keys so that they transmit
CTRL-A, then the normal character (controlled by shift) then CR
This means that CTRL-A can no longer be used for VEDSCREENLEFT
However, VEDREADFUNCTIONKEY has been defined so that three CTRL-A's in
succession will do screen left!


To Enter commands use the Page Erase key.
(FUNC and RETURN, or FUNC and ENTER will also work).

FUNC a does go to top of file
FUNC z does go to end of file
FUNC g does REDO last command as does FUNC -

There are the following minor differences between ved and ded (11/34)

Change case is obtained with CNTRL/N (not CNTRL/C)
FUNCT 5 goes to the middle of the text
ESC 5 puts the cursor in the middle of the line but doesn't scroll



A few VED procedures need to be re-defined.
*/


section;

define lconstant vedreadfunctionkey();
    ;;; when the FUNCTION button is depressed, any other key causes
    ;;; transmission of CTRL-A, then the normal code, then RETURN
    ;;; This procedure is invoked when CTRL-A is read in
    lvars vedchar;
    vedinascii() -> vedchar;    ;;; Read the character
    erase(vedinascii());    ;;; read carriage return, but ignore it

    switchon vedchar ==
    case `7  then vedcharupleft()    ;;; Keypad + FUNCT still does
        ;;; diagonal moves
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
    case `g then vedredokey();      ;;; redo last command
    case `- then vedredokey();
    case `\^M then vedenterkey();       ;;; treat return key as ENTER
    case `@ then vedclearhead();        ;;; F1
    case `A then vedlinedelete();       ;;; F2
    case `B then vedcleartail();        ;;; F3
    case `C then vedwordleftdelete();   ;;; F4
    case `D then vedwordrightdelete();  ;;; F5
    case `E then vedsetstatic();        ;;; F6
    case `F then vedmarklo();           ;;; F7
    case `G then vedmarkhi();           ;;; F8
    case `H then ved_m();               ;;; F9
    case `I then vedpushkey();          ;;; F10
    case `J then vedpopkey();           ;;; F11
    else vedscreenbell()
    endswitchon
enddefine;

define vedtvi925keys();

    vedset keys
        charup          = ctrl K
        charleft        = ctrl H
        charright       = ctrl L
        chardown        = ctrl V
        charupleftlots  = esc 7
        charuplots      = esc 8
        charuprightlots = esc 9
        charleftlots    = esc 4
        charrightlots   = esc 6
        chardownleftlots = esc 1
        chardownlots    = esc 2
        chardownrightlots = esc 3
        textleft        = esc ,
        textleft        = esc ctrl H        ;;; LEFT ARROW
        textright       = esc .
        textright       = esc ctrl L        ;;; RIGHT ARROW
        lineabove       = esc Q             ;;; char insert key
        linebelow       = esc E             ;;; line insert key
        screenup        = esc ctrl K        ;;; UP ARROW
        screendown      = esc ctrl V        ;;; DOWN ARROW
        dotdelete       = esc W             ;;; char delete key
        linedelete      = esc R             ;;; delete line key
        "enterkey"      = esc Y             ;;; Key above the 8 on keypad
        readfunctionkey = ctrl A            ;;; initiate function key sequence
        statusswitch    = esc cr
        screenmiddle    = esc 5
        refresh         = esc T             ;;; Key above the 7 on keypad
    endvedset

enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- Jason Handby, Jul 12 1988 - split into several files and changed to use
    vedset notation
--- Ben Rubinstein, Oct 12 1986 - vedenter, vedredo indirected through ..key
--- Aaron Sloman, Sep  5 1986 replaced cancel with sysunprotect
--- Roger Sinnhuber, June 1984 - customised.
*/
