/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.unix/lib/auto/sysobeylist.p
 >  Purpose:        RUN A LIST OF SHELL COMMANDS
 >  Author:         John Gibson, Nov 1982 (see revisions)
 >  Documentation:  HELP * SYSOBEYLIST
 >  Related Files:
 */
compile_mode:pop11 +strict;

section;

define global sysobeylist(list);
    lvars item list;
    for item in list do
        unless isstring(item) do
            mishap(item, 1, 'STRING NEEDED')
        endunless;
        sysobey(item, `!`)
    endfor
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- John Gibson, Oct 10 1992
        Added strict
--- John Williams, Mar 1983 - tidied up
--- Aaron Sloman, Dec 1983 - patched for use with Unix.
 */
