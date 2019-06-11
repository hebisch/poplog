/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/ved/vedmarkclasses.p
 > Purpose:         Named classes of marked range
 > Author:          John Gibson, Mar 13 1992 (see revisions)
 > Documentation:   REF *VEDVARS
 */
compile_mode :pop11 +strict;

section;

#_IF DEF vedprocess or DEF POPC_COMPILING

define lconstant mark_window();
    vedjumpto(vedlineoffset +1, 1);
    vedmarklo();
    vedscreendown();
    vedmarkhi();
enddefine;

vars vedmarkclasses = [
        line            ^(vedmarklo <> vedmarkhi)
        procedure       ^(valof(% "ved_mcp" %) <> apply)
        paragraph       ^vedmarkparagraph
        tostartfile     ^(valof(% "ved_mbf" %) <> apply)
        toendfile       ^(valof(% "ved_mef" %) <> apply)
        window          ^mark_window,
        file            ^(valof(% "ved_mbe" %) <> apply)
        start           ^vedmarklo
        lo              ^vedmarklo
        end             ^vedmarkhi
        hi              ^vedmarkhi
    ];

#_ELSE

    ;;; This is so XVed vedxvedmouse.p can get a list of the names
    ;;; when compiled with POPC.

vars vedmarkclasses = [
        line            ^identfn
        procedure       ^identfn
        paragraph       ^identfn
        tostartfile     ^identfn
        toendfile       ^identfn
        window          ^identfn
        file            ^identfn
        start           ^identfn
        lo              ^identfn
        end             ^identfn
        hi              ^identfn
    ];

#_ENDIF

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jan 18 1994
        vedmarkparagraph now globally defined
--- John Gibson, Jun  7 1993
        Moved vedmarkclasses in from ved_mark_named_range.p and added
        alternate list of names for use when compiled in safepop11
 */
