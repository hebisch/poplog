/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 >  File:           C.all/lib/ved/vedbacklocate.p
 >  Purpose:        searches back for occurrence of string
 >  Author:         Aaron Sloman, Jan 1985 (see revisions)
 >  Documentation:  REF OBSOLETE
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vedbacklocate(string);
    lvars string, flag;
    if isword(string) or isnumber(string) then
        string sys_>< nullstring -> string;
        [back nowrap noembed]
    else
        [back nowrap]
    endif -> flag;

    ved_check_search(string, flag);
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Sep 29 1993
        rewritten to use ved_check_search.
--- Aaron Sloman, Feb 15 1987
    Coped with '@a<string>'  not finding string right at beginning of file
--- Aaron Sloman, Jan 1985 - Fixed so that it really does find the most recent
    occrrence.  Also patterns can be used
    If the argument is a word or inteteger, then only exact matches are
    allowed.
 */
