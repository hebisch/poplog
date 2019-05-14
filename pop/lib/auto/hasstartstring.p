/* --- Copyright University of Sussex 1987.  All rights reserved. ---------
 | File:           $usepop/master/C.all/lib/auto/hasstartstring.p
 | Purpose:        Check if one string is initial substring of another
 | Author:         A.Sloman Mar 1 1987 (see revisions)
 | Documentation:  HELP * ISSTARTSTRING, REF * STRINGS
 | Related Files:
 */

section;

define global procedure hasstartstring(string,substr);
    lvars string,substr;
    isstartstring(substr,string)
enddefine;

endsection;
