/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/auto/wordswith.p
 >  Purpose:        return a sorted list of words containing the word or string
 >  Author:         Aaron Sloman, June 1982
 >  Documentation:  HELP * WORDSWITH
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define global wordswith(item);
dlvars item;
    sort(
        mapdic(
            procedure(word);
            lvars word;
                if issubstring(item, 1, word) then word endif
            endprocedure))
enddefine;

endsection;
