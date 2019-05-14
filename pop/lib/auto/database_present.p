/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 >  File:           C.all/lib/auto/database_present.p
 >  Purpose:        Returns true iff its argument is in the database
 >  Author:         John Gibson, Dec 27 1995
 >  Documentation:  REF * DATABASE
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define database_present(item);
    lvars item, l;
    if lmember_=(item, database) ->> l then
        fast_front(l) -> database_it;
        true
    else
        false
    endif
enddefine;

endsection;
