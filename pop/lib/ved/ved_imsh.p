/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:           C.unix/lib/ved/ved_imsh.p
 > Purpose:        Run sh from immediate mode ved file
 > Author:         A. Sloman August 1986 (see revisions)
 > Documentation:  HELP * IMSH, HELP * CSH_COMPILE
 > Related Files:  LIB * SH_COMPILE, * VED_SHFILE
*/
compile_mode :pop11 +strict;

section;

define vars ved_imsh;
    vedimshell(ved_shfile)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 13 1993
        Removed compiler argument -- not needed now
--- John Gibson, Nov 28 1992
        Made an ordinary procedure
--- John Gibson, Nov  1 1990
        Now uses -vedimshell-
 */
