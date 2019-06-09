/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XLINK_EXLIBDIRS.p
 > Purpose:         External library dirs for X link (for POPC only)
 > Author:          John Gibson, May 10 1993
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING  ;;; for compile-time use only

section;

;;; This is always defined in system linked for X
;;; POP_XLINK_EXLIBDIRS is a dummy env var defined by poplink

vars XLINK_EXLIBDIRS = ['==POP_XLINK_EXLIBDIRS'];

endsection;
