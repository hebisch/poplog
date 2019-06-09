/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.x/x/ved/auto/vedsunpwmxvedkeys.p
 >  Purpose:        Set Sun XVED Keyboard Same As vedsunkeys_SUNVIEW
 >  Author:         Julian Clinton, 27 May 1992 (see revisions)
 >  Documentation:  SYSDOC *XVED, HELP *SUNKEYS_SUNVIEW
 >  Related Files:  xvedrawin.p xvedkey.p
 */
compile_mode :pop11 +strict;

section;

define vedsunpwmxvedkeys;
    vedset keys
        pushkey             =   (F12)               ;;; L2 (F12)
        "redokey"           =   (F13)               ;;; L3
        popkey              =   (F14)               ;;; L4

        ;;; default SunView/XView bindings
        selection_copy      =   (L6)
        selection_paste     =   (L8)
        selection_cut       =   (L10)

        "ENTER crm"         =   esc (F12)           ;;; ESC L2 (F12)
        "ENTER break"       =   esc (F13)           ;;; ESC L3
        "refresh"           =   esc (F14)           ;;; ESC L4

        "screenbell"        =   (F1)                ;;; F1
        statusswitch        =   (F2)                ;;; F2
        clearhead           =   (F3)                ;;; F3
        linedelete          =   (F4)                ;;; F4
        cleartail           =   (F5)                ;;; F5
        marklo              =   (F6)                ;;; F6
        markhi              =   (F7)                ;;; F7
        ENTER m             =   (F8)                ;;; F8
        ENTER t             =   (F9)                ;;; F9
        "screenbell"        =   (F10)               ;;; F10 (type 4 kbd only)
        "screenbell"        =   (F11)               ;;; F11 (type 4 kbd only)

        "screenbell"        =   esc (F1)            ;;; ESC F1
        "ENTER yankw"       =   esc (F2)            ;;; ESC F2
        "ENTER yankw"       =   esc (F3)            ;;; ESC F3
        "ENTER yankl"       =   esc (F4)            ;;; ESC F4
        "ENTER yankw"       =   esc (F5)            ;;; ESC F5
        markfind            =   esc (F6)            ;;; ESC F6
        endrange            =   esc (F7)            ;;; ESC F7
        ENTER mi            =   esc (F8)            ;;; ESC F8
        ENTER ti            =   esc (F9)            ;;; ESC F9
        "screenbell"        =   esc (F10)           ;;; ESC F10 (type 4 kbd only)
        "screenbell"        =   esc (F11)           ;;; ESC F11 (type 4 kbd only)

        topfile             =   (F21)                 ;;; R1
        screenup            =   (F22)                 ;;; R2
        endfile             =   (F23)                 ;;; R3
        screenleft          =   (F24)                 ;;; R4
        screendown          =   (F25)                 ;;; R5
        textright           =   (F26)                 ;;; R6
        wordleft            =   (F27)                 ;;; R7
        wordright           =   (F29)                 ;;; R9
        "midwindow"         =   (F31)                 ;;; R11
        wordleft            =   (F33)                 ;;; R13
        "enterkey"          =   (F35)                 ;;; R15
        wordleft            =   (Insert)              ;;; Sun 4 only
        wordright           =   (KP_Decimal)          ;;; Sun 4 only

        lineabove           =   esc (F21)             ;;; ESC R1
        topfile             =   esc (F22)             ;;; ESC R2
        linebelow           =   esc (F23)             ;;; ESC R3
        textleft            =   esc (F24)             ;;; ESC R4
        endfile             =   esc (F25)             ;;; ESC R5
        screenright         =   esc (F26)             ;;; ESC R6
        charupleftlots      =   esc (F27)             ;;; ESC R7
        charuprightlots     =   esc (F29)             ;;; ESC R9
        "midwindow"         =   esc (F31)             ;;; ESC R11
        chardownleftlots    =   esc (F33)             ;;; ESC R13
        chardownrightlots   =   esc (F35)             ;;; ESC R15

    /*  Extra bindings for type 4 keyboard */

        "screenbell"        =   (Num_Lock)            ;;; NumLock

    endvedset;

    'sunkeys_sunview' -> vedkeymapname
enddefine;
;;;
uses-by_name (vedsunpwmxvedkeys);

#_IF pop_runtime
    vedsunpwmxvedkeys -> vedserverxvedkeys;
#_ENDIF

endsection;

/* --- Revision History ---------------------------------------------------
 */
