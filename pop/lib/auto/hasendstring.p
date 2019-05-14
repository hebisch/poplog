/* --- Copyright University of Sussex 1987.  All rights reserved. ---------
 | File:           $usepop/master/C.all/lib/auto/hasendstring.p
 | Purpose:        Check if one string is final substring of another
 | Author:         A.Sloman Mar 1 1987 (see revisions)
 | Documentation:  HELP * ISENDSTRING, REF * STRINGS
 | Related Files:
 */

section;

define global procedure hasendstring(string,substr);
    ;;; true iff substr is final substring of string
    ;;; works on words or strings
    lvars l1, l2, substr, string ;
    datalength(substr) -> l1;
    datalength(string) -> l2;
    if l2 fi_< l1 then false
    else issubstring(substr, l2 fi_- l1 fi_+ 1, string)
    endif
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Apr 10 1988
    Defined directly in terms of issubstring_lim, instead of in terms
    of isendstring
 */
