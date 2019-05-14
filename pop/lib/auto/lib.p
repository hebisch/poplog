/* --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.all/lib/auto/lib.p
 > Purpose:        Macro for loading library files.
 > Author:         Aaron Sloman, 1982 (see revisions)
 > Documentation:  HELP * LIB
 > Related Files:  LIB * LIBWARNING, LIB * LOADLIB, LIB * USES
 */

compile_mode:pop11 +strict;

section;

define global syntax lib;
    lvars file;
    dlocal popnewline = true;
    rdstringto([; ^termin ^newline]) -> file;
    sysPUSHQ(file);
    sysCALL("libwarning");
    sysPUSHQ(file);
    sysCALL("loadlib");
    ";" :: proglist -> proglist
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Mar 19 1992
        Now uses variable procedure -libwarning- to print message
--- John Gibson, Aug 14 1989
        Made a syntax word.
 */
