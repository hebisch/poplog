/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 >  File:           C.all/lib/auto/database_lookup.p
 >  Purpose:        finds a database element matching its argument
 >  Author:         John Gibson, Dec 27 1995
 >  Documentation:  REF * DATABASE
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define database_lookup(item);
    lvars item;
    unless database_present(item) then
        mishap(item, 1, 'DATABASE LOOKUP FAILED')
    endunless
enddefine;

endsection;
