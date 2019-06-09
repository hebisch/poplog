/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XTBASELIBS.p
 > Purpose:         Old X library library list
 > Author:          John Gibson, Mar 27 1993
 > Documentation:
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING  ;;; for compile-time use only

uses XTLIBFILES;

section;

identof("XLINK_EXLIBS") -> identof("XTBASELIBS");

endsection;
