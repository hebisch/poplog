/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/newsarray.p
 > Purpose:         Create a string array
 > Author:          John Gibson, Jun 24 1987 (see revisions)
 > Documentation:   REF *ARRAYS
 > Related Files:
 */

compile_mode :pop11 +strict;

section;

define global newsarray =
    newanyarray(% string_key %)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Dec 12 1995
        Uses string_key rather than datakey('').
 */
