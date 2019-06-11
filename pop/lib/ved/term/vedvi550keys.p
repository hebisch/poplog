/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/term/vedvi550keys.p
 >  Purpose:        lib v500 actually for Visual 550.- liable to change
 >  Author:         Aaron Sloman, Nov 1983 (see revisions)
 >  Documentation:  HELP * V550
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vedvi550keys();

    vedset keys
        charup              = esc [ A
        charleft            = esc [ D
        charright           = esc [ C
        chardown            = esc [ B
        wordleft            = esc 0
        wordright           = esc .
        charupleftlots      = esc [ 4 h
        charupleftlots      = esc [ 4 l
        charuplots          = esc 8
        charuplots          = esc [ K
        charuprightlots     = esc [ P
        charleftlots        = esc [ N
        charrightlots       = esc [ J
        chardownleftlots    = esc [ L
        chardownlots        = esc 2
        chardownrightlots   = esc [ M
        textright           = esc esc [ C
        lineabove           = esc esc esc [ D
        linebelow           = esc esc esc [ C
        screenup            = esc esc [ A
        screenleft          = esc esc [ D
        screendown          = esc esc [ B
        topfile             = esc esc esc [ A
        endfile             = esc esc esc [ B
        dotdelete           = esc ctrl ?
        "enterkey"          = esc ctrl M
        "enterkey"          = esc [ H
        "redokey"           = esc -
        statusswitch        = esc ,
    endvedset

enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Jason Handby, Sep 14 1989
        Moved out of old "v550.p" and adapted to use vedset notation.
*/
