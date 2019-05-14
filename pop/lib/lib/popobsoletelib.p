/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/lib/popobsoletelib.p
 > Purpose:         Extend popuseslist to include obsolete things
 > Author:          John Gibson, Nov 11 1992
 > Documentation:   REF *popobsoletelib
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

extend_searchlist('$usepop/pop/lib/obsolete/' dir_>< nullstring,
                                popuseslist, true) -> popuseslist;

global constant popobsoletelib = true;

endsection;
