/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/check_string.p
 > Purpose:         Test for a string
 > Author:          Aaron Sloman, May 23 1990 (see revisions)
 > Documentation:   REF * STRINGS/check_string
 > Related Files:
 */
compile_mode :pop11 +strict :vm -pentch;

section;

define check_string(obj) with_props false;
    lvars obj;
    unless isstring(obj) then
        mishap(obj,1,'STRING NEEDED');
    endunless
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep  3 1992
        Added compile_mode
 */
