/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/auto/countwords.p
 >  Purpose:        count the number of words in the dictionary;
 >  Author:         Mark Rubinstein, Sep 25 1985 (see revisions)
 >  Documentation:
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define global countwords() -> n;
dlvars n = 0;
    appdic(procedure(); erase(); n + 1 -> n endprocedure);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 10 1992
        Cleaned up
 */
