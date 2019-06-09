/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 >  File:           C.x/x/ved/auto/vedsunxvedkeys.p
 >  Purpose:        Set VED for XVED Keyboard
 >  Author:         Jonathan Meyer, 20 July 1990 (see revisions)
 >  Documentation:  SYSDOC *XVED, HELP *SUNXVEDKEYS
 >  Related Files:  xvedrawin.p xvedkey.p
 */
compile_mode :pop11 +strict;

section;

define vedsunxvedkeys();
    vedset keys
        "screenbell"        =   (F11)               ;;; L1 (F11)
        lineabove           =   (F12)               ;;; L2 (F12)
        pushkey             =   (F13)               ;;; L3
        linebelow           =   (F14)               ;;; L4
        exchangeposition    =   (F15)               ;;; L5
        popkey              =   (F17)               ;;; L7
        statusswitch        =   (F19)               ;;; L9

        "screenbell"        =   esc (F11)           ;;; ESC L1 (F11)
        "ENTER crm"         =   esc (F12)           ;;; ESC L2 (F12)
        "ENTER break"       =   esc (F13)           ;;; ESC L3
        "refresh"           =   esc (F14)           ;;; ESC L4
        ENTER jp            =   esc (F15)           ;;; ESC L5
        setstatic           =   esc (F16)           ;;; ESC L6
        ENTER pop           =   esc (F17)           ;;; ESC L7

        marklo              =   (F1)                ;;; F1
        markhi              =   (F2)                ;;; F2
        clearhead           =   (F3)                ;;; F3
        linedelete          =   (F4)                ;;; F4
        cleartail           =   (F5)                ;;; F5
        wordleftdelete      =   (F6)                ;;; F6
        wordrightdelete     =   (F7)                ;;; F7
        ENTER m             =   (F8)                ;;; F8
        ENTER t             =   (F9)                ;;; F9
        "screenbell"        =   (F10)               ;;; F10 (type 4 kbd only)

        ENTER mbf           =   esc (F1)            ;;; ESC F1
        ENTER mef           =   esc (F2)            ;;; ESC F2
        "ENTER yankw"       =   esc (F3)            ;;; ESC F3
        "ENTER yankl"       =   esc (F4)            ;;; ESC F4
        "ENTER yankw"       =   esc (F5)            ;;; ESC F5
        "ENTER yankw"       =   esc (F6)            ;;; ESC F6
        "ENTER yankw"       =   esc (F7)            ;;; ESC F7
        ENTER mi            =   esc (F8)            ;;; ESC F8
        ENTER ti            =   esc (F9)            ;;; ESC F9
        "screenbell"        =   esc (F10)           ;;; F10 (type 4 kbd only)


        textleft            =   (F21)                 ;;; R1
        screenup            =   (F22)                 ;;; R2
        textright           =   (F23)                 ;;; R3
        screenleft          =   (F24)                 ;;; R4
        screendown          =   (F25)                 ;;; R5
        screenright         =   (F26)                 ;;; R6
        charupleft          =   (F27)                 ;;; R7
        charupright         =   (F29)                 ;;; R9
        "ENTER timed_esc"   =   (F31)                 ;;; R11
        wordleft            =   (F33)                 ;;; R13
        wordright           =   (F35)                 ;;; R15
        wordleft            =   (Insert)              ;;; R13
        wordright           =   (KP_Decimal)          ;;; R15

        "topwindow"         =   esc (F21)             ;;; ESC R1
        topfile             =   esc (F22)             ;;; ESC R2
        markfind            =   esc (F23)             ;;; ESC R3
        "midwindow"         =   esc (F24)             ;;; ESC R4
        endfile             =   esc (F25)             ;;; ESC R5
        "endrange"          =   esc (F26)             ;;; ESC R6
        charupleftlots      =   esc (F27)             ;;; ESC R7
        charuprightlots     =   esc (F29)             ;;; ESC R9
        "midwindow"         =   esc (F31)             ;;; ESC R11
        chardownleftlots    =   esc (F33)             ;;; ESC R13
        chardownrightlots   =   esc (F35)             ;;; ESC R15

    /*  Extra bindings for type 4 keyboard */

        "screenbell"        =   (Num_Lock)            ;;; NumLock

    /* Extra bindings for type 5 keyboard
        (these are copied from LIB VEDNCDXVEDKEYS
    */

        /* These two can't be copied because they would overwrite the
            use of the Insert key symbol as WORDLEFT, above.
        lineabove           = (Insert)                ;;; Insert
        linebelow           = esc (Insert)            ;;; ESC Insert
        */

        "prevscreen"        = (Prior)                 ;;; Page Up
        "nextscreen"        = (Next)                  ;;; Page Down
        topfile             = (Home)                  ;;; Home
        endfile             = (End)                   ;;; End
        ENTER xup           = esc (Prior)             ;;; ESC Page Up
        ENTER xdn           = esc (Next)              ;;; ESC Page Down
        markfind            = esc (Home)              ;;; ESC Home
        endrange            = esc (End)               ;;; ESC End

    endvedset;

    if xved_value("application", "EditKeysEnabled") then
        ;;; bind the Cut/Copy/Paste functions
        vedset keys
            selection_copy      = (L6)
            selection_paste     = (L8)
            selection_cut       = (L10)
        endvedset;
    else
        vedset keys
            "loadline"          =   (L6)               ;;; L6
            "redokey"           =   (L8)               ;;; L8
            "enterkey"          =   (L10)              ;;; L10
        endvedset;
    endif;

    'sunxved' -> vedkeymapname
enddefine;
;;;
uses-by_name (vedsunxvedkeys);


#_IF pop_runtime
    vedsunxvedkeys -> vedserverxvedkeys;
#_ENDIF

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Apr 20 1993
        Added key mappings for Sun type 5 keyboard
--- Jonathan Meyer, Jul 31 1991
        Made it use compile mode strict
--- Adrian Howard, Jun 19 1991
        Removed delete & backspace bindings since they are now set by default
        with LIB *VEDXVEDKEYS.
        Added reference to HELP *SUNXVEDKEYS
--- Jonathan Meyer, Apr  6 1991
        Changed to use xved_set_keys and keysym names
 */
