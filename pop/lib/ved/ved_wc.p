/*  --- Copyright University of Sussex 1987. All rights reserved. ----------
 >  File:           C.all/lib/ved/ved_wc.p
 >  Purpose:        word count (uses vedwordcount_text)
 >  Author:         Tom Khabaza, March 20 1985 (see revisions)
 >  Documentation:  HELP * WC
 >  Related Files:  LIB * VEDWORDCOUT_TEXT
 */
compile_mode :pop11 +strict;

section;

define vars ved_wc;
    if vvedbuffersize == 0 then
        vedputmessage('File is empty.')
    else
        vedwordcount_text(1, vvedbuffersize);
    endif;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Roger Evans, Oct  2 1987 fixed ved_wc to check for empty buffer
 */
