/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/ved/term/vedvt52screen.p
 >  Purpose:        set ved for a 'DUMB' vt52 compatible terminal.
 >  Author:         Aaron Sloman, Jan 1983 (see revisions)
 >  Documentation:  HELP * VT52
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vedvt52screen();

    "vt52" -> vedterminalname;
    false -> vedterminalselect;

    vedvt52screenxy -> vedscreenxy;
    vedvt52screengraphtrans -> vedscreengraphtrans;

    vedset screen
        charleft    = esc D
        charright   = esc C
        charup      = esc A
        point       = esc Y
        cleartail   = esc K
        clear       = esc J
    endvedset

enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb  7 1992
        Added assignment to vedscreengraphtrans
--- Jason Handby, Rob Duncan, Oct 23 1989
        Adapted from old "vedvt52.p" to use new vedset notation.
*/
