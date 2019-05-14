/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/lib/auto/ismidstring.p
 > Purpose:         Test for proper substring
 > Author:          Aaron Sloman, Apr  3 1988 (see revisions)
 > Documentation:   HELP * STRINGS
 > Related Files:   LIB * HASMIDSTRING, HASSTARTSTRING, ISENDSTRING, etc.
 */
compile_mode:pop11 +strict;

;;; like issubstring, but string1 must not be an initial or final substring
;;; of string2
section;

define global ismidstring(string1,string2);
    lvars
        string1,string2,
        lim=(max(1,datalength(string2) fi_- 1));
    issubstring_lim(string1,2,false,lim,string2)
enddefine;

endsection;
