/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvi200keys.p
 > Purpose:         set up the key bindings for vi200 terminal
 > Author:          Aaron Sloman & John Gibson (see revisions)
 > Related Files:   vedvi200.p vedvi200screen.p
 */
compile_mode :pop11 +strict;

section;

;;; Unprotect "vedvi200keys" in case we're loading this file in a system
;;; where it's already built in
sysunprotect("vedvi200keys");

define vedvi200keys();

;;; --- MAPPING BETWEEN CHARACTERS AND PROCEDURES ------------------------

    vedset keys
    ;;;keypad keys
    ;;;small moves
        charup            = esc ? x
        charupright       = esc ? y
        charright         = esc ? v
        chardownright     = esc ? s
        chardown          = esc ? r
        chardownleft      = esc ? q
        charleft          = esc ? t
        charupleft        = esc ? w
        charmiddle        = esc ? u

    ;;;bigish moves
        wordleft          = esc ? l
        wordright         = esc ? n

    ;;;miscellaneous
        statusswitch      = esc ? p
        "enterkey"        = esc ? M
        "redokey"         = esc ? m
        ENTER xdn         = esc ^J
        ENTER xup         = esc ^H

    ;;;function key bindings
    ;;;very big moves
        screenleft        = esc D
        textright         = esc C
        screenup          = esc A
        screendown        = esc B
        endfile           = esc H
        topfile           = esc t

    ;;;small delete
        dotdelete         = esc P

    ;;;bigish delete
        clearhead         = esc Q
        linedelete        = esc R
        cleartail         = esc sp
        wordleftdelete    = esc !
        wordrightdelete   = esc "

    ;;;miscellaneous
        setstatic         = esc #
        marklo            = esc $
        markhi            = esc %
        ENTER m           = esc &
        pushkey           = esc '\''
        popkey            = esc (
        lineabove         = esc )
        linebelow         = esc *
        refresh           = esc v
    endvedset;

enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Rob Duncan, Oct  9 1989
        Sectionised
--- Andreas Schoter, Jul 12 1989
        re-writen to use vedset syntax and screen stuff seperated out
--- John Gibson, Dec 16 1987
        Tidied up.
 */
