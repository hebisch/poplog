/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 >  File:           C.all/lib/ved/vedkeyproc.p
 >  Purpose:        given a string return pdr ved would run if chars were typed
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vedkeyproc(string) -> proc;
    lvars string char len proc;
    dlocal ved_char_in_stream;
    datalist(string) <> [1 1 1 1 1] -> ved_char_in_stream;
    dest(ved_char_in_stream) -> ved_char_in_stream -> char;
    vedgetproctable(char) -> proc;
    listlength(ved_char_in_stream) - 5 -> len;
    if len /== 0 then
        vederror(
            if len > 0 then len >< ' SURPLUS CHARACTERS in '
            else -len >< ' CHARACTERS MISSING in ' endif
            >< string)
    endif;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 27 1995
        Tidied
 */
