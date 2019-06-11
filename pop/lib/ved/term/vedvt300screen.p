/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvt300screen.p
 > Purpose:         Configure VED for VT300 type terminal
 > Author:          Robert John Duncan, Oct 22 1990 (see revisions)
 > Documentation:
 > Related Files:   LIB * VEDVT220SCREEN, * VEDVT300KEYS
 */
compile_mode :pop11 +strict;

section;

uses vedvt220screen;

weak constant procedure vedtermcapscreen;

define vedvt300screen();
    false -> vedterminalname;
    if testdef vedtermcapscreen
    and (weakref vedtermcapscreen('vt300'); vedterminalname)
    then
        vedset screen;
            graphic         = esc ( 0
            alpha           = esc ( B
            charnormal      = esc [ m
            charbold        = esc [ 1 m
            charunderline   = esc [ 4 m
            charblink       = esc [ 5 m
            charhighlight   = esc [ 7 m
        endvedset;
        vedvt100screengraphtrans -> vedscreengraphtrans;
    else
        vedvt220screen();
    endif;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 15 1992
        Replaced #_IF with testdef on vedtermcapscreen
--- John Gibson, Feb  8 1992
        Added char- sequences; added assignment to vedscreengraphtrans
        and removed setting of graph char variables
 */
