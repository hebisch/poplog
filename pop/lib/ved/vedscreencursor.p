/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/vedscreencursor.p
 > Purpose:         Old name for vedscreenstatus_-_mark/vedscreencursorlinemark
 > Author:          John Gibson, Jan 24 1992
 > Documentation:   REF *OBSOLETE
 */

#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +strict;

section;

define global active vedscreencursor;
    ;;; return this one as the other may be in bold
    vedscreenstatus_-_mark
enddefine;
;;;
define updaterof active vedscreencursor char;
    lvars char;
    char ->> vedscreenstatus_-_mark -> vedscreencursorlinemark
enddefine;


endsection;
