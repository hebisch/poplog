/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:           C.unix/lib/ved/ved_imcsh.p
 > Purpose:        Run csh from immediate mode ved file
 > Author:         John Gibson (new version), June 1984 (see revisions)
 > Documentation:  HELP * CSH_COMPILE
 > Related Files:  LIB * CSH_COMPILE, LIB * VED_CSHFILE, LIB * VED_IMSH
 */
compile_mode :pop11 +strict;

section;

define vars ved_imcsh;
    vedimshell(ved_cshfile)
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
