/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/ved/auto/xved_seln_undo.p
 > Purpose:         Old name for XVed vedselection_ procedure
 > Author:          John Gibson, Jun  4 1993
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

constant procedure xved_seln_undo = vedselection_undo;

endsection;
