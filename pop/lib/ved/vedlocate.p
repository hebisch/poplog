/*  --- Copyright University of Sussex 1993. All rights reserved. ----------
 >  File:           C.all/lib/ved/vedlocate.p
 >  Purpose:        search for a string or word.  If word insist on item boundaries.
 >  Author:         (updated) Aaron Sloman, April 1985 (see revisions)
 >  Documentation:  REF OBSOLETE
 >  Related Files:
 */
#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +strict;

section;

define vedlocate(string);
    lvars string;
    ved_check_search(
        string, unless isstring(string) then () >< nullstring endunless,
        (isword(string) or isinteger(string)) and [noembed] or []);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Sep 29 1993
        Rewritten to use ved_check_search
 */
