/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/ved/vedloadline.p
 >  Purpose:        make <ESC>-d load current line
 >  Author:         Aaron Sloman, Jan 1985 (see revisions)
 >  Documentation:  HELP * VEDPROCS/vedloadline
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vedloadline();
;;; compile current line
    vedmarkpush();
    true -> vvedmarkprops;
    vedmarkpush();
    false -> vvedmarkprops;
    vedmarkhi();
    vedmarklo();
    ved_lmr();
    vedmarkpop();
    vedmarkpop();
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Nov 17 1992
        Removed key mapping (unnecessary, since name is mapped to key
        by default).
--- Mark Rubinstein, Feb 1985 - modified using VEDMARKPUSH and VEDMARKPOP.
    Had to do true -> vvedmarkprops because ved_lmr calls vedcleartempmark
 */
