/* --- Copyright University of Sussex 1987.  All rights reserved. ---------
 | File:           $usepop/master/C.all/lib/auto/hassubstring.p
 | Purpose:        Check if one string is substring of another
 | Author:         A.Sloman Feb 16 1987 (see revisions)
 | Documentation:  HELP * ISSUBSTRING, REF * STRINGS
 | Related Files:  LIB * ISENDSTRING, * HASENDSTSRING, * ISMIDSTRING
 */

section;

define global procedure hassubstring(string,substr) with_nargs 2;
    ;;; integer argument is optional
    lvars substr,int,string;
    if isinteger(string) then string -> int; -> string
    else 1 -> int
    endif;
    issubstring_lim(substr, int, false, false, string)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Apr 10 1988
    re-defined in terms of issubstring_lim (like issubstring). Also
    allowed the integer to be optional.
 */
