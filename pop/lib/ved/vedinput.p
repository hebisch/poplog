/*  --- Copyright University of Sussex 1991. All rights reserved. ----------
 >  File:           C.all/lib/ved/vedinput.p
 >  Purpose:        Add something to VED's input stream
 >  Author:         Aaron Sloman, April 1982 (see revisions)
 >  Documentation:  HELP * VEDINPUT
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

weak constant procedure XptSetXtWakeup;

define vedinput(x);
    lvars x;
    if x.islist then
        x <> ved_char_in_stream
    else
        x :: ved_char_in_stream
    endif -> ved_char_in_stream;
    if testdef XptSetXtWakeup and vedusewindows == "x" and not(vedinvedprocess)
    then
        weakref XptSetXtWakeup();
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Aug 30 1991 Allowed lists
--- John Gibson, Aug 30 1991
        Tidied up
--- Jonathan Meyer, Jun 22 1991
        Adjusted to test for XptSetXtWakeup
 */
