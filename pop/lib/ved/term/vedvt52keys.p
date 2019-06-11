/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/ved/term/vedvt52keys.p
 >  Purpose:        set ved for a 'DUMB' vt52 compatible terminal.
 >  Author:         Aaron Sloman, Jan 1983 (see revisions)
 >  Documentation:  HELP * VT52
 >  Related Files:  LIB * VEDVT52SCREEN
 */
compile_mode :pop11 +strict;

section;

define vedvt52keys();

    vedset keys
        charup                  = esc A         ;;; arrow keys
        charleft                = esc D
        charright               = esc C
        chardown                = esc B
        wordleft                = esc ,
        wordleft                = esc esc D
        wordright               = esc .
        wordright               = esc esc C
        charupleftlots          = esc 7
        charuplots              = esc 8
        charuprightlots         = esc 9
        charleftlots            = esc 4
        charrightlots           = esc 6
        chardownleftlots        = esc 1
        chardownlots            = esc 2
        chardownrightlots       = esc 3
        textright               = esc P esc C
        lineabove               = esc P esc Q
        linebelow               = esc P esc R
        screenup                = esc P esc A
        screenleft              = esc P esc D
        screendown              = esc P esc B
        topfile                 = esc esc A     ;;; ESC up-arrow
        endfile                 = esc esc B     ;;; ESC down-arrow
        dotdelete               = esc P 8
        chardelete              = esc P 7
        wordleftdelete          = esc P ,
        wordrightdelete         = esc P .
        clearhead               = esc P 4
        cleartail               = esc P 6
        linedelete              = esc P 5
        "enterkey"              = esc e
        "redokey"               = esc cr
        pushkey                 = esc P 1
        popkey                  = esc P 2
        "enterkey"              = esc P cr
        "redokey"               = esc P lf
        ENTER m                 = esc esc Q     ;;; ESC PF2
        ENTER t                 = esc esc R     ;;; ESC PF3
        marklo                  = esc Q         ;;; PF2
        markhi                  = esc R         ;;; PF3
        statusswitch            = esc 0
        charmiddle              = esc 5
        refresh                 = esc P esc P   ;;; PF1 PF1
        charright <> dotdelete  = esc P 9
        exchangeposition        = esc P 3
    endvedset

enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, May 29 1992
        Changed \r and \n to cr and lf respectively (cf BR geraldg.1)
--- Jason Handby, Aug 3 1989
    Altered to use vedset notation, split into separate files
--- Ben Rubinstein, Oct 12 1986  - vedenter, vedredo indirected through ..key
*/
