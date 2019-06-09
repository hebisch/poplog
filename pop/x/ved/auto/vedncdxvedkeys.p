/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/ved/auto/vedncdxvedkeys.p
 > Purpose:         VED: key bindings for NCD X terminal
 > Author:          Rob Smith, Sep 14 1989 (see revisions)
 > Documentation:   HELP *NCDXVEDKEYS
 > Related Files:   C.x/x/ved/lib/vedxvedkeys.p
 */
compile_mode :pop11 +strict;

/*
    This file defines the procedure -vedncdxtermkeys- which customises
    VED's key bindings for the keyboard of an NCD16 X terminal
 */

section;

define vedncdxvedkeys();
    vedset keys
        screenup            = (Up)
        screendown          = (Down)
        screenleft          = (Left)
        textright           = (Right)

        topfile             = esc (Up)
        endfile             = esc (Down)
        textleft            = esc (Left)
        screenright         = esc (Right)

        dotdelete           = (F1)

        lineabove           = (Insert)
        "prevscreen"        = (Prior)
        "nextscreen"        = (Next)
        topfile             = (Home)
        endfile             = (End)
        linebelow           = esc (Insert)
        ENTER xup           = esc (Prior)
        ENTER xdn           = esc (Next)
        markfind            = esc (Home)
        endrange            = esc (End)

        ENTER timed_esc     = (KP_5)
        "helpkey"           = (KP_Divide)
        "loadline"          = (KP_Multiply)

        "ENTER hkey"        = esc (KP_Divide)
        ENTER lmr           = esc (KP_Multiply)

    endvedset;
    'ncdxved' -> vedkeymapname;
    true -> $-xved$-xvedkeypadon;
enddefine;
;;;
uses-by_name (vedncdxvedkeys);

#_IF pop_runtime
    vedncdxvedkeys -> vedserverxvedkeys;
#_ENDIF

endsection;


/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jul 31 1991
        Made it use compile mode strict
--- Adrian Howard, Jun 19 1991 : Removed KP_Add definition, now in
        LIB *VEDXVEDKEYS
--- Adrian Howard, Jun 19 1991 : F1 now bound to dotdelete for consistancy with
        old ved, reference to HELP *NCDXVEDKEYS added
--- Jonathan Meyer, Jun  2 1991
        Added -xvedkeypadon- to force keypads to send escape sequences
--- Jonathan Meyer, Apr  8 1991
        Converted to xved style
--- John Williams, Oct 12 1990
        -vedkeymapname- instead of -vedkeyboardname-
--- John Williams, Oct  5 1990
        Now uses -vedhelpkey-, and sets -vedkeyboardname-
--- John Williams & Rob Duncan, Nov  8 1989
        Function key definitions changed to be more similar to
        LIB VEDVT220KEYS
--- Rob Duncan, Oct  9 1989
        Sectionised; added call to -vedvt100keys-
--- Andreas Schoter Sep 1989
        Modified to use the -vedset- notation
 */
