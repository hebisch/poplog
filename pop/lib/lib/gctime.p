/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:           C.all/lib/lib/gctime.p
 > Purpose:        measure garbage collection time.
 > Author:         A. Sloman 1982 (see revisions)
 > Documentation:  HELP * TIME /GCTIME
 > Related Files:  LIB * TIME
 */
compile_mode :pop11 +strict;

uses time;

section;

define vars syntax gctime;
    dlocal popgctrace = true;
    nonsyntax time();
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 23 1992
        Made syntax
--- Aaron Sloman, Jan 26 1986 - Revised to use lconstant, instead of a
    section. Also now works in VED with LMR.
*/
