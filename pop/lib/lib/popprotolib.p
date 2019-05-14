/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lib/popprotolib.p
 > Purpose:         Extend popuseslist to include prototype libraries
 > Author:          Simon Nichols, Oct 18 1993
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

extend_searchlist('$usepop/pop/lib/proto/' dir_>< nullstring,
                    popuseslist, true) -> popuseslist;

global constant popprotolib = true;

endsection;
