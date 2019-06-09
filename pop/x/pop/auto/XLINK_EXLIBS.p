/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XLINK_EXLIBS.p
 > Purpose:         External library list for X link
 > Author:          John Gibson, May 10 1993
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING  ;;; for compile-time use only

section;

vars XLINK_EXLIBS = [% ident XLINK_EXLIBDIRS, ident XLINK_EXLIBFILES %];

endsection;
