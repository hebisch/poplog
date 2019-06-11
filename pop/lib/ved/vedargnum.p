/*  --- Copyright University of Sussex 1989. All rights reserved. ----------
 >  File:           C.all/lib/ved/vedargnum.p
 >  Purpose:        returns a number corresponding to VEDARUGMENT (defaults to 1)
 >  Author:         Aaron Sloman, July 1982 (see revisions)
 >  Documentation:
 >  Related Files:  Used in various VED commands. Also LIB VEDARGINT
 */
compile_mode :pop11 +strict;

section;

define vedargnum(arg) -> arg;
    lvars arg;
    unless strnumber(arg) ->> arg then
        1 -> arg
    endunless;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Nov 11 1989
    Slightly tidied. Also defined LIB VEDARGINT
--- Mark Rubinstein, Oct  4 1985 - sectionised and lvarsed.
 */
