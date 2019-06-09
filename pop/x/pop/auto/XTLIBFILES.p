/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XTLIBFILES.p
 > Purpose:         Old X library library list
 > Author:          John Gibson, Mar 27 1993
 > Documentation:
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING  ;;; for compile-time use only

include sysdefs.ph;

section;

#_IF DEF UNIX and DEF XHASOLDX
    XLINK_EXLIBFILES <> ['-loldX'] -> XLINK_EXLIBFILES;
#_ENDIF

identof("XLINK_EXLIBFILES") -> identof("XTLIBFILES");

endsection;
