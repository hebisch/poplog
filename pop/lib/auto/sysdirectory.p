/* --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.all/lib/auto/sysdirectory.p
 > Purpose:        Replaced by -current_directory- from Version 12.3
 > Author:         John Gibson, Oct 13 1986 (see revisions)
 > Documentation:  HELP * CURRENT_DIRECTORY
 > Related Files:
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

define global sysdirectory = nonactive current_directory(%%) enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 10 1992
        Made a closure
 */
