/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           C.all/lib/ved/ved_qshowlib.p
 > Purpose:        Quit current file and call SHOWLIB on another
 > Author:         John Williams, Jul 26 1985 (see revisions)
 > Documentation:  HELP * VEDCOMMS /ved_qshowlib
 > Related Files:  LIB * VEDQGET
 */
compile_mode :pop11 +strict;

section;

define vars ved_qshowlib();
    vedqget(ved_showlib)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Ben Rubinstein, Apr 30 1986 - removed 'uses vedqget' (now in system)
*/
