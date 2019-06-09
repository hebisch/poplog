/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XLINK_EXT_EXLIBS.p
 > Purpose:         Extended library list for X external load
 > Author:          John Gibson, May 10 1993
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING  ;;; for compile-time use only

include sysdefs.ph;

section;

vars XLINK_EXT_EXLIBS =

#_IF DEF UNIX
    ['-lXmu' '-lXext']

#_ELSE
    ['decw$xmulibshr/share' 'decw$xextlibshr/share']

#_ENDIF
;


endsection;
