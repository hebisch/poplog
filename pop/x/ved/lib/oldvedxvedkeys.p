/* --- Copyright University of Sussex 2003. All rights reserved. ----------
 > File:            C.x/x/ved/lib/oldvedxvedkeys.p
 > Purpose:         OLD Key bindings for XVed terminal emulator
 >                  Replaced by new version 26 Jul 2003 [A.Sloman]
 > Author:          Jon Meyer, April 1 1991(see revisions)
 > Documentation:   HELP * XVED, HELP * XVEDKEYS
 > Related Files:   LIB * VEDHPXVEDKEYS, * VEDSUNXVEDKEYS, * VEDVT220KEYS
 */
compile_mode :pop11 +strict;

/*
    Choose a keyboard configuration for an XVed terminal emulator.

    This is highly site-specific, since xveds may run on various hosts,
    all with different keyboards. The method given here simply chooses
    between HP, DEC and Sun keyboards depending on the host on which the
    library itself is loaded: this will fail if the terminal emulator
    is running remotely on a different type of machine.

    Sites requiring a more complex selection mechanism can provide a
    local version of this file in POPLOCALAUTO. See HELP * VEDXVED for
    more details.
 */

include xved_constants;

section $-xved => oldveddefaultxvedkeys, oldvedxvedkeys;

/* Generic XVed keyboard settings - these will be the same for all
   X platforms. Some of the keys are bound onto things that will be
   changed by machine-specific libraries
*/

/*
define lconstant vedtoggle_menubar;
    not(xved_value("currentWindow", "menubarOn")) ->
                xved_value("currentWindow", "menubarOn");
enddefine;
*/

define lconstant vedtoggle_scrollbar;
    not(xved_value("currentWindow", "scrollbarOn")) ->
                xved_value("currentWindow", "scrollbarOn");
enddefine;

define vars oldveddefaultxvedkeys();

    ;;; set xved default key sequences

    vedset keys
        dointerrupt         =   ctrl c                ;;; Interrupt binding
        chardelete          =   ctrl ?
        dotdelete           =   ctrl h
        chardelete          =   (Delete)              ;;; Delete
        dotdelete           =   (BackSpace)           ;;; Back Space

        charup              =   (Up)                  ;;; Up
        chardown            =   (Down)                ;;; Down
        charleft            =   (Left)                ;;; Left
        charright           =   (Right)               ;;; Right

        charuplots          =   esc (Up)              ;;; ESC Up
        chardownlots        =   esc (Down)            ;;; ESC Down
        charleftlots        =   esc (Left)            ;;; ESC Left
        charrightlots       =   esc (Right)           ;;; ESC Right

        "helpkey"           =   (Help)                ;;; Help
        "ENTER hkey"        =   esc (Help)            ;;; ESC Help
        wordleft            =   (KP_0)                ;;; KP 0
        wordright           =   (KP_Decimal)          ;;; KP .
        "enterkey"          =   (KP_Enter)            ;;; KP Enter
        statusswitch        =   (KP_Add)              ;;; KP +
        "redokey"           =   (KP_Subtract)         ;;; KP -

        ;;; VED DEFAULT KEYPAD

        charup              =   (KP_8)
        chardown            =   (KP_2)
        charleft            =   (KP_4)
        charright           =   (KP_6)
        charuplots          =   esc (KP_8)
        chardownlots        =   esc (KP_2)
        charleftlots        =   esc (KP_4)
        charrightlots       =   esc (KP_6)
        charupleft          =   (KP_7)
        charupright         =   (KP_9)
        chardownleft        =   (KP_1)
        chardownright       =   (KP_3)
        charupleftlots      =   esc (KP_7)
        charuprightlots     =   esc (KP_9)
        chardownleftlots    =   esc (KP_1)
        chardownrightlots   =   esc (KP_3)

        screenup            =   (Prior)
        topfile             =   esc (Prior)
        screendown          =   (Next)
        endfile             =   esc (Next)

        textleft            =   (Home)
        screenleft          =   esc (Home)
        textright           =   (End)
        screenright         =   esc (End)

        "ENTER static"      =   (Insert)

        ;;; Standard 12-function key keyboard functions
        "enterkey"          = (F1)
        clearhead           = (F2)
        linedelete          = (F3)
        cleartail           = (F4)

        wordleftdelete      = (F5)
        wordrightdelete     = (F6)
        marklo              = (F7)
        markhi              = (F8)

        ENTER m             = (F9)
        ENTER t             = (F10)
        pushkey             = (F11)
        exchangeposition    = (F12)

        "refresh"           = esc (F1)
        "ENTER yankw"       = esc (F2)
        "ENTER yankl"       = esc (F3)
        "ENTER yankw"       = esc (F4)

        "ENTER yankw"       = esc (F5)
        "ENTER yankw"       = esc (F6)
        ENTER mbf           = esc (F7)
        ENTER mef           = esc (F8)

        ENTER mi            = esc (F9)
        ENTER ti            = esc (F10)
        popkey              = esc (F11)
        "ENTER cps"         = esc (F12)

        selection_cut       = (Meta) x
        selection_copy      = (Meta) c
        selection_paste     = (Meta) v
        selection_help      = (Meta) h
        selection_compile   = (Meta) d
        ;;; toggle_menubar      = (Meta) m
        toggle_scrollbar    = (Meta) s
    endvedset;

    if xved_value("application", "EditKeysEnabled") then
        vedset keys
            selection_help  =   (Help)                ;;; Help
        endvedset;
    endif;

    'xved' -> vedkeymapname;
enddefine;

define vars oldvedxvedkeys();
    oldveddefaultxvedkeys();
    vedserverxvedkeys();
enddefine;
;;;
uses-by_name (vedxvedkeys);

endsection;


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jul 26 2003
        Old version of vedxvedkeys after rationalising the F1 to F10 keys
--- John Gibson, Mar  1 1996
        Moved to x/ved/lib
--- Jonathan Meyer, Jan 13 1992 fixed spelling of scrollbar in
        vedtoggle_scrollbar
--- Integral Solutions Ltd, Sep 18 1991 (Julian Clinton)
        Removed toggle_menubar.
--- Jonathan Meyer, Sep 12 1991 Added toggle_menubar/scrollbar
--- Adrian Howard, Aug  2 1991 : Stopped -ved_cps- autoloading early
--- Jonathan Meyer, Jul  8 1991
        Added interrupt binding
--- Adrian Howard, Jun 19 1991 : Swapped delete & backspace around and added
        reference to HELP *XVEDKEYS
--- Jonathan Meyer, Jun 17 1991
        Added vedxvedsetwindow
--- Jonathan Meyer, Apr  6 1991 Changed to use xved_set_keys and keysym names

 */
