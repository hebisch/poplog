/* --- Copyright University of Sussex 1994. All rights reserved. ------
 > File:            C.all/lib/ved/ved_sgsr.p
 > Purpose:         "Silent" global substitute in a range
 > Author:          Aaron Sloman, Apr 18 1992 (see revisions)
 > Documentation:   REF * VEDSEARCH
 > Related Files:   LIB * VED_SGS, SRC * VDREGEXP.P
 */

section;

compile_mode :pop11 +strict;

define global vars ved_sgsr =
    ved_search_or_substitute(%'range -ask -verbose -here', true%)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan 12 1994
        Rewritten to use ved_search_or_substitute.
 */
