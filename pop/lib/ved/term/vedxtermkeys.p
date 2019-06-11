/* --- Copyright University of Birmingham 2003. All rights reserved. ------
 > --- Copyright University of Sussex 2003. All rights reserved. ----------
 > File:            C.all/pop/lib/ved/term/vedxtermkeys.p
 > Purpose:         Key bindings for standard PC-like keyboard, used in xterm
 >                      Also works for NCD xterminals
 > Author:          Aaron Sloman, Oct 13 1995 (see revisions)
 > Documentation:   HELP * VEDXTERMKEYS, HELP * VEDXTERM
 > Related Files:
 */


;;; Based on this:
/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedxtermkeys.p
 > Purpose:         VED: key bindings for NCD X terminal
 > Author:          Rob Smith, Sep 14 1989 (see revisions)
 > Related Files:
 */

/* This file defines the procedure -vedxtermkeys- which customises

    In order to use the keys marked "Home" and "End" in VED, the following
    key translations must be added to they user's .Xdefaults file, or to
    some system-wide file setting Xterm defaults, e.g.
        /usr/X11R6/lib/X11/app-defaults/XTerm
    or possibly
        /usr/lib/X11/app-defaults/XTerm

    ! xterm translations to make VED work
    XTerm*VT100*Translations: #override \
        <Key>KP_0: string(0x1b) string("Op") \n\
        <Key>KP_Insert: string(0x1b) string("Op") \n\
        <Key>KP_Decimal: string(0x1b) string("On") \n\
        <Key>KP_Delete: string(0x1b) string("On") \n\
        <Key>KP_Home: string(0x1b) string("Ow") \n\
        <Key>KP_7: string(0x1b) string("Ow") \n\
        <Key>KP_Prior: string(0x1b) string("Oy") \n\
        <Key>KP_9: string(0x1b) string("Oy") \n\
        <Key>KP_Next: string(0x1b) string("Os") \n\
        <Key>KP_1: string(0x1b) string("Oq") \n\
        <Key>KP_End: string(0x1b) string("Oq") \n\
        <Key>KP_3: string(0x1b) string("Os")
    XTerm*deleteIsDEL: true

    The Keypad 0 and . keys will transmit Op and On respectively,
    and other numeric keypad keys that default to the same settings as
    central keypad keys are now given different strings to transmit,
    modelled on vt100 keys.
    
    Delete will transmit the delete character.

Invoke the following with
        vedxtermkeys();
*/

section;

;;; vedexpandchar is defined in vdprocess.p and defaults to false
;;; Make it work as in HELP VEDEXPAND for XVed users.

`^` -> vedexpandchar;

uses vedvt100keys;

define global vedxtermkeys();
    vedvt100keys();
    vedset keys

        ;;; Two utility functions that probably should be provided in the
        ;;; system sources, i.e. in vedescapetable in SRC vdkeys.p
        ;;; Mnemonic: ESC -  keep one copy of marked range and move it here
        ;;;           ESC =  make two copies, by copying to here

        "ENTER m"           = esc -             ;;; ESC -
        "ENTER t"           = esc =             ;;; ESC =

        ;;; Function key mappings follow:


        ;;; Arrow keys
        "charup"            = esc [ A           ;;; UP
        "chardown"          = esc [ B           ;;; DOWN
        "charleft"          = esc [ D           ;;; LEFT
        "charright"         = esc [ C           ;;; RIGHT

/*
    Old version of the following
        "topfile"           = esc esc [ A       ;;; ESC UP
        "endfile"           = esc esc [ B       ;;; ESC DOWN
        "textleft"          = esc esc [ D       ;;; ESC LEFT
        "screenright"       = esc esc [ C       ;;; ESC RIGHT
*/

        "charuplots"        = esc esc [ A       ;;; ESC UP
        "chardownlots"      = esc esc [ B       ;;; ESC DOWN
        "charleftlots"      = esc esc [ D       ;;; ESC LEFT
        "charrightlots"     = esc esc [ C       ;;; ESC RIGHT

/*
    ;;; Possible options for esc A, esc B, esc C, esc D
        "screenup"          = esc  A            ;;; UP
        "screendown"        = esc  B            ;;; DOWN
        "screenleft"        = esc  D            ;;; LEFT
        "textright"         = esc  C            ;;; RIGHT

        "topfile"           = esc esc  A        ;;; ESC UP
        "endfile"           = esc esc  B        ;;; ESC DOWN
        "textleft"          = esc esc  D        ;;; ESC LEFT
        "screenright"       = esc esc  C        ;;; ESC RIGHT
*/

        ;;; Function keys F1 to F12

        "marklo"            = esc [ 1 1 ~       ;;; F1
        "marklo"            = esc O P           ;;; F1 (on PC)
        "markhi"            = esc [ 1 2 ~       ;;; F2
        "markhi"            = esc O Q           ;;; F2 (on PC)
        "clearhead"         = esc [ 1 3 ~       ;;; F3
        "clearhead"         = esc O R           ;;; F3 (on PC)
        "linedelete"        = esc [ 1 4 ~       ;;; F4
        "linedelete"        = esc O S           ;;; F4 (on PC)

        "cleartail"         = esc [ 1 5 ~       ;;; F5
        "wordleftdelete"    = esc [ 1 7 ~       ;;; F6
        "wordrightdelete"   = esc [ 1 8 ~       ;;; F7
        "ENTER m"           = esc [ 1 9 ~       ;;; F8

        "ENTER t"           = esc [ 2 0 ~       ;;; F9
        "xrefresh"          = esc [ 2 1 ~       ;;; F10
        "pushkey"           = esc [ 2 3 ~       ;;; F11
        "exchangeposition"  = esc [ 2 4 ~       ;;; F12

        "ENTER mbf"         = esc esc [ 1 1 ~   ;;; ESC F1
        "ENTER mbf"         = esc esc O P       ;;; ESC F1 (on PC)

        "ENTER mef"         = esc esc [ 1 2 ~   ;;; ESC F2
        "ENTER mef"         = esc esc O Q       ;;; ESC F2 (On PC)

        "ENTER yankw"       = esc esc [ 1 3 ~   ;;; ESC F3
        "ENTER yankw"       = esc esc O R       ;;; ESC F3 (On PC)

        "ENTER yankl"       = esc esc [ 1 4 ~   ;;; ESC F4
        "ENTER yankl"       = esc esc O S       ;;; ESC F4 (On PC)

        "ENTER yankw"       = esc esc [ 1 5 ~   ;;; ESC F5
        "ENTER yankw"       = esc esc [ 1 7 ~   ;;; ESC F6
        "ENTER yankw"       = esc esc [ 1 8 ~   ;;; ESC F7
        "ENTER mi"          = esc esc [ 1 9 ~   ;;; ESC F8

        "ENTER ti"          = esc esc [ 2 0 ~   ;;; ESC F9
        "ENTER mo"          = esc esc [ 2 1 ~   ;;; ESC F10
        "popkey"            = esc esc [ 2 3 ~   ;;; ESC F11
        "ENTER cps"         = esc esc [ 2 4 ~   ;;; ESC F12

        ;;; Middle function keypad
        "joinline"      = esc ^?            ;;; ESC BS/ESC Delete
        ;;; see HELP vedxtermkeys
        "ENTER static"       = esc [ 2 ~        ;;; INSERT
        "lineabove"          = esc esc [ 2 ~    ;;; ESC INSERT

        "topfile"           = esc [ H           ;;; HOME
        "endfile"           = esc [ F         ;;; END
        "markfind"          = esc esc [ H       ;;; ESC HOME
        "endrange"          = esc esc [ F       ;;; ESC END

        "topfile"           = esc [ 1 ~         ;;; HOME
        "endfile"           = esc [ 4 ~         ;;; END
        "markfind"          = esc esc [ 1 ~     ;;; ESC HOME
        "endrange"          = esc esc [ 4 ~     ;;; ESC END

        ;;; For people with older settings for .Xdefaults
        "topfile"           = esc <             ;;; HOME
        "endfile"           = esc >             ;;; END
        "markfind"          = esc esc <         ;;; ESC HOME
        "endrange"          = esc esc >         ;;; ESC END

        "prevscreen"        = esc [ 5 ~         ;;; PAGE UP (Prior)
        "nextscreen"        = esc [ 6 ~         ;;; PAGE DOWN (Next)

        "ENTER xup"         = esc esc [ 5 ~     ;;; ESC PAGE UP
        "ENTER xdn"         = esc esc [ 6 ~     ;;; ESC PAGE DOWN


        ;;; Numeric keypad keys
        "enter"             = esc O M           ;;; ENTER key on right

        "statusswitch"      = esc O k           ;;; Keypad +

        "redocommand"       = esc O m           ;;; Keypad *

        "helpkey"           = esc O o           ;;; Keypad / (Invokes ENTER hkeys)
        "ENTER hkey"        = esc esc O o       ;;; ESC Keypad /

        ;;; unusable sequences because they duplicate the central keypad
        ;;; esc [ 2 ~  ;;; Keypad 0 key (Duplicates Central Insert key)
        ;;; esc [ 3 ~  ;;; Keypad Del key (Duplicates central Delete key)
        ;;; esc [ 4 ~  ;;; Keypad 1  End
        ;;; esc [ B    ;;; Keypad 2  Down
        ;;; esc [ 6 ~  ;;; Keypad 3  PageDown (Next)
        ;;; esc [ D    ;;; Keypad 4  Left
        ;;; esc [ C    ;;; Keypad 6  Right
        ;;; esc [ 1 ~  ;;; Keypad 7  Home
        ;;; esc [ A    ;;; Keypad 8  Up
        ;;; esc [ 5 ~  ;;; Keypad 9  PageUp (Prior)

        ;;; These MAY work in some Xterm configurations
        ;;; Especially if .Xdefaults is set right
        "charupleft"        = esc O w           ;;; Keypad 7
        "charupleftlots"    = esc esc O w       ;;; ESC Keypad 7

        ;;; These depend on entries in .Xdefaults
        "charupright"       = esc O y           ;;; Keypad 9
        "charuprightlots"   = esc esc O y       ;;; ESC Keypad 9
        "chardownright"     = esc O s           ;;; Keypad 9
        "chardownrightlots" = esc esc O s       ;;; ESC Keypad 9

        "midwindow"         = esc [ E           ;;; Keypad 5
        "midwindow"         = esc O u           ;;; Keypad 5


        "chardownleft"      = esc O q           ;;; Keypad 1
        "chardownleftlots"  = esc esc O q       ;;; ESC Keypad 1

        "wordleft"          = esc [ O p         ;;; Keypad 0 key (Ins)
        "wordright"         = esc [ O n         ;;; Keypad . key (Del)

        "loadline"          = esc O j           ;;; Keypad *
        "ENTER lmr"         = esc esc O j       ;;; ESC Keypad *


    /*
        ;;; DO WE NEED THIS?
        ;;; EXTRAS FOR Xterminal VERSION N-123UK
        ;;; The "/" key = R5, and the "*" key = R6

        "textleft"          =   esc [ 1 1 1 ~       ;;; R1  Pause
        "screenup"          =   esc [ 1 1 2 ~       ;;; R2  PrScr
        "textright"         =   esc [ 1 1 3 ~       ;;; R3  ScrLock
        "redocommand"       =   esc [ 1 1 4 ~       ;;; R4  Pad "-"
        "screendown"        =   esc [ 1 1 5 ~       ;;; R5  "/"
        "screenright"       =   esc [ 1 1 7 ~       ;;; R6  "*"
        "charupleft"        =   esc [ 1 1 8 ~       ;;; R7  (7)
        "charup"            =   esc [ 1 1 9 ~       ;;; R8  (8)
        "charupright"       =   esc [ 1 2 0 ~       ;;; R9  (9)
        "charleft"          =   esc [ 1 2 1 ~       ;;; R10 (4)
        "midwindow"         =   esc [ 1 2 3 ~       ;;; R11 (5)
        "charright"         =   esc [ 1 2 4 ~       ;;; R12 (6)
        "chardownleft"      =   esc [ 1 2 5 ~       ;;; R13 (1)
        "chardown"          =   esc [ 1 2 6 ~       ;;; R14 (2)
        "chardownright"     =   esc [ 1 2 8 ~       ;;; R15 (3)

        "topwindow"         =   esc esc [ 1 1 1 ~   ;;; ESC R1 Pause
        "topfile"           =   esc esc [ 1 1 2 ~   ;;; ESC R2 PrScr
        "markfind"          =   esc esc [ 1 1 3 ~   ;;; ESC R3 ScrLock
        "ENTER lmr"         =   esc esc [ 1 1 4 ~   ;;; ESC keypad "="
        "endfile"           =   esc esc [ 1 1 5 ~   ;;; ESC R5 "/" and Top Right
        "ENTER lmr"         =   esc esc [ 1 1 7 ~   ;;; ESC R6 "*"
        "charupleftlots"    =   esc esc [ 1 1 8 ~   ;;; ESC R7 (7)
        "charuplots"        =   esc esc [ 1 1 9 ~   ;;; ESC R8 (8)
        "charuprightlots"   =   esc esc [ 1 2 0 ~   ;;; ESC R9 (9)
        "charleftlots"      =   esc esc [ 1 2 1 ~   ;;; ESC R1 (4)
        "midwindow"         =   esc esc [ 1 2 3 ~   ;;; ESC R1 (5)
        "charrightlots"     =   esc esc [ 1 2 4 ~   ;;; ESC R1 (6)
        "chardownleftlots"  =   esc esc [ 1 2 5 ~   ;;; ESC R1 (1)
        "chardownlots"      =   esc esc [ 1 2 6 ~   ;;; ESC R1 (2)
        "chardownrightlots" =   esc esc [ 1 2 8 ~   ;;; ESC R1 (3)
    */

    endvedset;


    if systranslate('LINUX') then
        vedset keys
            ;;; Stuff for linux console
            "marklo" = '\^[[[A'
            "markhi" = '\^[[[B'
            "clearhead" = '\^[[[C'
            "deleteline" = '\^[[[D'
            "cleartail" = '\^[[[E'
            "enter"     =  '\^[[1~'
            "topfile"   =  '\^[[2~'
            "chardelete" = '\^[[3~'
            "endfile"   =  '\^[[4~'
        endvedset;
    endif;

    vedsetkey('\^L', "vedxrefresh");
    vedsetkey('\^H', "vedchardelete");
    
    ;;; this is used by ENTER hkeys
    'vedxterm' -> vedkeymapname;
enddefine;



endsection;


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, 27 Jul 2003
    A large collection of changes partly to make this and LIB VEDXVEDKEYS
    more similar, partly to rationalise things that don't work in xterms
    on linux PCs because of different keysym names e.g. KP_Home rather than
    KP_7. This made it necessary to revise the .Xdefault settings required,
    as explained above. New keybindings documented in HELP VEDXTERMKEYS

--- Aaron Sloman, Jan 11 2003
        Made backspace chardelete by default
--- Aaron Sloman, Sep 26 2002
        Put in more options for linux+PC
--- Aaron Sloman, Jun 19 1997
    Set CTRL-L to vedxrefresh
--- Aaron Sloman, Dec 24 1996
    Extended for NCD keyboard type N-123UK
--- Aaron Sloman, Sep  9 1996
    Alas, had to make keypad and big pad arrows both move in small
    steps, for DEC Alpha
--- Aaron Sloman, Oct 1995
    Put all procedure names in quotes. Also added Motif Xved bindings.
    Removed top leve call of vedxtermkeys.
--- Aaron Sloman, Oct 1992
--- Aaron Sloman, Aug 19 1992
        Made More consistent with SunKeys, by changing
        F1 to F12 mappings
--- John Williams, Aug 18 1992
        Added key mappings for the HOME and END keys
--- John Williams, Oct 12 1990
        -vedkeymapname- instead of -vedkeyboardname-
--- John Williams, Oct  5 1990
        Now uses -vedhelpkey-, and sets -vedkeyboardname-
--- John Williams & Rob Duncan, Nov  8 1989
        Function key definitions changed to be more similar to
        LIB VEDVT220KEYS
--- Rob Duncan, Oct 9 1989
        Sectionised; added call to -vedvt100keys-
--- Andreas Schoter Sep 1989
        Modified to use the -vedset- notation
 */
