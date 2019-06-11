/*  --- Copyright University of Sussex 1996.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_deof.p
 >  Purpose:        delete to end of file
 >  Author:         Aaron Sloman, May 1983 (see revisions)
 >  Documentation:  REF * VEDCOMMS
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vars ved_deof();
    lvars lastline = max(vvedmarkhi, vvedbuffersize);
    if vedline <= lastline then
        vedmarkpush(), false -> vvedmarkprops;
        vedline -> vvedmarklo;
        lastline -> vvedmarkhi;
        ved_d();
        vedmarkpop()
    else
        vedenderror()
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 10 1996
        Rewritten to correct bugs.
--- John Gibson, May 14 1989
        Replaced procedure-local -vars- with -dlocal-
*/
