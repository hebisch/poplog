/* --- Copyright University of Sussex 2003. All rights reserved. ----------
 > File:            C.x/x/ved/lib/vedxvedkeys.p
 > Purpose:         Key bindings for XVed terminal emulator
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

;;; vedexpandchar is defined in vdprocess.p and defaults to false
;;; Make it work as in HELP VEDEXPAND for XVed users.

`^` -> vedexpandchar;

include xved_constants;

uses vedjoinline;
uses vedxtermkeys;

section $-xved vedxtermkeys => veddefaultxvedkeys, vedxvedkeys;

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

define vars veddefaultxvedkeys();

    ;;; set xved default key sequences

    vedset keys

        ;;; Two utility functions that probably should be provided in the
        ;;; main system sources, i.e. in vedescapetable in SRC vdkeys.p
        ;;; Mnemonic: ESC -  keep one copy of marked range and move it here
        ;;;           ESC =  make two copies, by copying to here

        "ENTER m"           = esc -                 ;;; ESC -
        "ENTER t"           = esc =                 ;;; ESC =

        ;;; These two produce common functionality found in other applications
        "ENTER w1"          = ^s                    ;;; Ctrl S (Save file)
        "ENTER q"           = ^q                    ;;; Ctrl Q (Quit file)

        dointerrupt         =   ctrl c                ;;; Interrupt binding

        ;;; change delete key to delete under cursor (alas)
        ;;; chardelete      =   ctrl ?
        "dotdelete"         =   ctrl ?
        ;;; chardelete      =   (Delete)              ;;; Delete
        "dotdelete"         =   (Delete)              ;;; Delete

        ;;; change backspace to delete to left
        ;;; dotdelete       =   ctrl h
        "chardelete"        =   ctrl h
        ;;; dotdelete       =   (BackSpace)           ;;; Back Space
        "chardelete"        =   (BackSpace)           ;;; Back Space

        ;;; The next item does not seem to work, so it is repeated
        ;;;  below usingvedsetkey
        "joinline"          =   esc (Delete) ;;; Join this line to previous

        ;;; Arrow keys
        "charup"            =   (Up)                  ;;; Up
        "chardown"          =   (Down)                ;;; Down
        "charleft"          =   (Left)                ;;; Left
        "charright"         =   (Right)               ;;; Right

        ;;; Arrow keys preceded by ESC
        "charuplots"        =   esc (Up)              ;;; ESC Up
        "chardownlots"      =   esc (Down)            ;;; ESC Down
        "charleftlots"      =   esc (Left)            ;;; ESC Left
        "charrightlots"     =   esc (Right)           ;;; ESC Right

        ;;; Top row of numeric keypad
        "redokey"           =   (KP_Subtract)         ;;; KP -
    
        ;;; Help key may not exist
        "helpkey"           =   (Help)                ;;; Help
        "ENTER hkey"        =   esc (Help)            ;;; ESC Help

        ;;; So use Keypad / Key
        "helpkey"           =   (KP_Divide)           ;;; KP /
        "ENTER hkey"        =   esc (KP_Divide)       ;;; ESC KP /

        ;;; Compile line or range using keypad * key
        "loadline"          =   (KP_Multiply)         ;;; KP *
        "ENTER lmr"         =   esc (KP_Multiply)     ;;; ESC KP *

        ;;; Other non-numbered keys on numeric keypad
        "statusswitch"      =   (KP_Add)              ;;; KP +
        "enterkey"          =   (KP_Enter)            ;;; KP Enter

        "wordleft"          =   (KP_0)                ;;; KP 0
        "wordleft"          =   (KP_Insert)           ;;; KP 0
        "wordright"         =   (KP_Decimal)          ;;; KP .
        "wordright"         =   (KP_Delete)           ;;; KP .

        ;;; VED DEFAULT Numeric KEYPAD
        "charup"            =   (KP_8)
        "chardown"          =   (KP_2)
        "charleft"          =   (KP_4)
        "charright"         =   (KP_6)

        "charuplots"        =   esc (KP_8)
        "chardownlots"      =   esc (KP_2)
        "charleftlots"      =   esc (KP_4)
        "charrightlots"     =   esc (KP_6)

        ;;; Alternative mapping
        "charup"            =   (KP_Up)
        "chardown"          =   (KP_Down)
        "charleft"          =   (KP_Left)
        "charright"         =   (KP_Right)

        "charuplots"        =   esc (KP_Up)
        "chardownlots"      =   esc (KP_Down)
        "charleftlots"      =   esc (KP_Left)
        "charrightlots"     =   esc (KP_Right)

        ;;; Diagonal moves
        "charupleft"        =   (KP_7)
        "charupright"       =   (KP_9)
        "chardownleft"      =   (KP_1)
        "chardownright"     =   (KP_3)

        "charupleftlots"    =   esc (KP_7)
        "charuprightlots"   =   esc (KP_9)
        "chardownleftlots"  =   esc (KP_1)
        "chardownrightlots" =   esc (KP_3)

        ;;; Alternative mappings for diagonal moves
        "charupleft"        =   (KP_Home)
        "charupright"       =   (KP_Prior)
        "chardownleft"      =   (KP_End)
        "chardownright"     =   (KP_Next)

        "charupleftlots"    =   esc (KP_Home)
        "charuprightlots"   =   esc (KP_Prior)
        "chardownleftlots"  =   esc (KP_End)
        "chardownrightlots" =   esc (KP_Next)

        ;;; Middle key
        "midwindow"         =   (KP_Begin)
        "midwindow"         =   (KP_5)

        ;;; Keypad above arrow keypad
        "ENTER static"      =   (Insert)        ;;; Insert key
        "lineabove"         =   esc (Insert)    ;;; ESC + Insert key
        ;;; Delete key already handled above.

        "topfile"           =   (Home)
        "markfind"          =   esc (Home)
        "endfile"           =   (End)
        "endrange"          =   esc (End)

        "prevscreen"        =   (Prior)         ;;; Page Up
        "ENTER xup"         =   esc (Prior)     ;;; ESC Page Up
        "nextscreen"        =   (Next)          ;;; Page Down
        "ENTER xdn"         =   esc (Next)      ;;; ESC Page Down


/*
        ""      = esc esc [ F F B E     ;;; ESC F1
        ""      = esc esc [ F F B F     ;;; ESC F2
        ""      = esc esc [ F F C 0     ;;; ESC F3
        ""      = esc esc [ F F C 1     ;;; ESC F4
        ""      = esc esc [ F F C 2     ;;; ESC F5
        ""      = esc esc [ F F C 3     ;;; ESC F6
        ""      = esc esc [ F F C 4     ;;; ESC F7
        ""      = esc esc [ F F C 5     ;;; ESC F8
        ""      = esc esc [ F F C 6     ;;; ESC F9
        ""      = esc esc [ F F C 7     ;;; ESC F10
        ""      = esc esc [ F F C 8     ;;; ESC F11
        ""      = esc esc [ F F C 9     ;;; ESC F12

*/

        ;;; F1 to F12 Standard 12-function key keyboard functions on top row
        ;;; Previous values are commented out 26 Jul 2003
        ;;; In case keysyms (F1) (F2) etc., don't work also include the
        ;;; character sequences, e.g. esc[[FFBE, etc.

;;;     "enterkey"          = (F1)
        "marklo"            = esc [ F F B E     ;;;  F1
        "marklo"            = (F1)


;;;     clearhead           = (F2)
        "markhi"            = esc [ F F B F     ;;; F2
        "markhi"            = (F2)

;;;     linedelete          = (F3)
        "clearhead"         = esc [ F F C 0     ;;; F3
        "clearhead"         = (F3)

;;;     cleartail           = (F4)
        "linedelete"        = esc [ F F C 1     ;;; F4
        "linedelete"        = (F4)

;;;     wordleftdelete      = (F5)
        "cleartail"         = esc [ F F C 2     ;;; F5
        "cleartail"         = (F5)

;;;     wordrightdelete     = (F6)
        "wordleftdelete"    = esc [ F F C 3     ;;; F6
        "wordleftdelete"    = (F6)

;;;     marklo              = (F7)
        "wordrightdelete"       = esc [ F F C 4     ;;; F7
        "wordrightdelete"   = (F7)

;;;     markhi              = (F8)
        "ENTER m"           = esc [ F F C 5     ;;; F8
        "ENTER m"           = (F8)

;;;     ENTER m             = (F9)
        "ENTER t"           = esc [ F F C 6     ;;; F9
        "ENTER t"           = (F9)

;;;     ENTER t             = (F10)
        "vedrefresh"        = esc [ F F C 7     ;;; F10
        "vedrefresh"        = (F10)

        "pushkey"           = esc [ F F C 8     ;;; F11
        "pushkey"           = (F11)

        "exchangeposition"  = esc [ F F C 9     ;;; F12
        "exchangeposition"  = (F12)

        ;;; F1 to F12 Keys preceded by ESC
;;;     "refresh"           = esc (F1)
        "ENTER mbf"         = esc esc [ F F B E     ;;; ESC F1
        "ENTER mbf"         = esc (F1)

;;;     "ENTER yankw"       = esc (F2)
        "ENTER mef"         = esc esc [ F F B F     ;;; ESC F2
        "ENTER mef"         = esc (F2)

        "ENTER yankw"       = esc esc [ F F C 0     ;;; ESC F3
        "ENTER yankw"       = esc (F3)

        "ENTER yankl"       = esc esc [ F F C 1     ;;; ESC F4
        "ENTER yankl"       = esc (F4)

        "ENTER yankw"       = esc esc [ F F C 2     ;;; ESC F5
        "ENTER yankw"       = esc (F5)

        "ENTER yankw"       = esc esc [ F F C 3     ;;; ESC F6
        "ENTER yankw"       = esc (F6)

;;;     ENTER mbf           = esc (F7)
        "ENTER yankw"       = esc esc [ F F C 4     ;;; ESC F7
        "ENTER yankw"       = esc (F7)

;;;     ENTER mef           = esc (F8)
        "ENTER mi"          = esc esc [ F F C 5     ;;; ESC F8
        ENTER mi            = esc (F8)

;;;     ENTER mi            = esc (F9)
        "ENTER ti"          = esc esc [ F F C 6     ;;; ESC F9
        ENTER ti            = esc (F9)

;;;     ENTER ti            = esc (F10)
        "ENTER mo"          = esc esc [ F F C 7     ;;; ESC F10
        ENTER mo            = esc (F10)

        "popkey"            = esc esc [ F F C 8     ;;; ESC F11
        "popkey"            = esc (F11)

        "ENTER cps"         = esc esc [ F F C 9     ;;; ESC F12
        "ENTER cps"         = esc (F12)


        selection_cut       = (Meta) x
        selection_copy      = (Meta) c
        selection_paste     = (Meta) v
        selection_help      = (Meta) h
        selection_compile   = (Meta) d
        ;;; toggle_menubar      = (Meta) m
        toggle_scrollbar    = (Meta) s
    endvedset;

    ;;; ESC deleted, or ESC backspace.
    vedsetkey('\^[\^?', vedjoinline);

    if xved_value("application", "EditKeysEnabled") then
        vedset keys
            selection_help  =   (Help)                ;;; Help
        endvedset;
    endif;

    'xved' -> vedkeymapname;
enddefine;

define vars vedxvedkeys();
    vedxtermkeys();
    veddefaultxvedkeys();
    vedserverxvedkeys();
enddefine;
;;;
uses-by_name (vedxvedkeys);

endsection;


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Aug  1 2003
    In case keysyms don't work, added definitions for function keys
    using the esc sequences generated by XVed.
--- Aaron Sloman, Jul 26 2003
        Revised default definition of keys F1 to F12
        Also swapped backspace and DEL to be more consistent with most other
           PC systems (alas)
        Made keypad keys work on xved on linux (had wrong keysyms, because
        there appear to be alternative names (as shown by xkeycaps). Now
        have entries for both names.
        Added vedwindowmiddle for keypad 5.
        Added vedjoinline for ESC delete
        Made esc Insert do vedlineabove
        Added non-false value for vedexpandchar
        Made more consistent with lib vedxtermkeys
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
