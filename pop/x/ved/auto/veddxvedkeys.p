/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/ved/auto/veddxvedkeys.p
 > Purpose:         VED key bindings for DECwindows DXterm terminal emulator
 > Author:          Rob Duncan, Apr 23 1990 (see revisions)
 */
compile_mode :pop11 +strict;


section;

define veddxvedkeys();
    vedset keys
        ;;; Select sends escape
        '\^[' = (Select)

        chardelete = ctrl ?
        ;;; F11 is ESC key, so move MARKHI to ESC F12
        markhi      = esc (F12)

        /* Make keypad 5 a timed escape key */
        ENTER timed_esc     = (KP_5)

        /* Arrow keys */
        screenup            = (Up)
        screendown          = (Down)
        textright           = (Right)
        screenleft          = (Left)

        topfile             = esc (Up)
        endfile             = esc (Down)
        screenright         = esc (Right)
        textleft            = esc (Left)

        /* Function keys */
        clearhead           = (F6)          ;;; F6
        linedelete          = (F7)          ;;; F7
        cleartail           = (F8)          ;;; F8
        wordleftdelete      = (F9)          ;;; F9
        wordrightdelete     = (F10)         ;;; F10

        "ENTER yankw"       = esc (F6)      ;;; ESC F6
        "ENTER yankl"       = esc (F7)      ;;; ESC F7
        "ENTER yankw"       = esc (F8)      ;;; ESC F8
        "ENTER yankw"       = esc (F9)      ;;; ESC F9
        "ENTER yankw"       = esc (F10)     ;;; ESC F10

        marklo              = (F11)         ;;; F11
        markhi              = (F12)         ;;; F12
        ENTER m             = (F13)         ;;; F13
        ENTER t             = (F14)         ;;; F14

        ENTER mbf           = esc (F11)     ;;; ESC F11
        ENTER mi            = esc (F13)     ;;; ESC F13
        ENTER ti            = esc (F14)     ;;; ESC F14

        pushkey             = (F17)         ;;; F17
        exchangeposition    = (F18)         ;;; F18
        setstatic           = (F19)         ;;; F19
        dotdelete           = (F20)         ;;; F20

        popkey              = esc (F17)     ;;; ESC F17
        "ENTER cps"         = esc (F18)     ;;; ESC F18
        "ENTER break"       = esc (F19)     ;;; ESC F19
        refresh             = esc (F20)     ;;; ESC F20

        /* Labelled keys (NB Select = Escape (see above)) */
        "helpkey"           = (Help)        ;;; Help
        "loadline"          = (Menu)        ;;; Do

        "ENTER hkey"        = esc (Help)    ;;; ESC Help
        ENTER lmr           = esc (Help)    ;;; ESC Do

        markfind            = (Find)            ;;; Find
        lineabove           = (Insert)          ;;; Insert Here
        "prevscreen"        = (Prior)           ;;; Prev Screen
        "nextscreen"        = (Next)            ;;; Next Screen

        "endrange"          = esc (Find)        ;;; ESC Find
        linebelow           = esc (Insert)      ;;; ESC Insert Here
        "screenbell"        = esc (Select)      ;;; ESC Select (= ESC ESC)
        ENTER xup           = esc (Prior)       ;;; ESC Prev Screen
        ENTER xdn           = esc (Next)        ;;; ESC Next Screen

        /* "gold" table entries (gold key = KeySym KP_F1) */
        "refresh"              = (KP_F1) (KP_F1)        ;;; PF1 PF1
        lineabove              = (KP_F1) (KP_F2)        ;;; PF1 PF2
        linebelow              = (KP_F1) (KP_F3)        ;;; PF1 PF3
        setstatic              = (KP_F1) (KP_F4)        ;;; PF1 PF4
        chardelete             = (KP_F1) (KP_7)
        dotdelete              = (KP_F1) (KP_8)
        charright <> dotdelete = (KP_F1) (KP_9)
        clearhead              = (KP_F1) (KP_4)
        linedelete             = (KP_F1) (KP_5)
        cleartail              = (KP_F1) (KP_6)
        pushkey                = (KP_F1) (KP_1)
        popkey                 = (KP_F1) (KP_2)
        exchangeposition       = (KP_F1) (KP_3)
        wordleftdelete         = (KP_F1) (KP_0)
        wordrightdelete        = (KP_F1) (KP_Decimal)   ;;; PF1 "."
        topfile                = (KP_F1) (Up)
        endfile                = (KP_F1) (Down)
        dotdelete              = (KP_F1) (Delete)

        /* keypad entries */
        switchstatus           = (KP_Separator)     ;;; Keypad ","
        marklo                 = (KP_F2)            ;;; PF2
        markhi                 = (KP_F3)            ;;; PF3

    endvedset;
enddefine;
;;;
uses-by_name (veddxvedkeys);

#_IF pop_runtime
    veddxvedkeys -> vedserverxvedkeys;
#_ENDIF

endsection;


/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Aug  2 1991 : Stopped -ved_cps- autoloading early
--- Jonathan Meyer, Jul 31 1991
        Made it use compile mode strict
--- Adrian Howard, Jul 15 1991
        - Added PF2 & PF3 defs
        - corrected ESC F12 to be -vedmarkhi-
--- Jonathan Meyer, Jun 28 1991
        Changed to use string on lhs of vedset rule for Select
--- Robert John Duncan, Jun 25 1991
        -xved_keysym_seq- in section "xved"; changed (Del) to (Delete)
--- Adrian Howard, Jun 20 1991 : Keypad "," set to vedswitchstatus
--- Adrian Howard, Jun 19 1991 : Added "gold" keys
--- Robert John Duncan, Oct 19 1990
        Added binding for xrefresh
 */
