/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/lib/auto/hasmidstring.p
 > Purpose:         check whether a string is a proper substring of another
 > Author:          Aaron Sloman, Apr  4 1988 (see revisions)
 > Documentation: HELP * HASMIDSTRING, REF * STRINGS
 > Related Files: LIB * ISMIDSTRING, * HASSTARTSTRING, * ISENDSTRING, etc.
 */

section;
define global procedure hasmidstring(string2,string1);
    ;;; like ismidstring, with the arguments reversed.
    lvars
        string1,string2,
        lim=(max(1,datalength(string2) fi_- 1));
    issubstring_lim(string1,2,false,lim,string2)
enddefine;

endsection;
