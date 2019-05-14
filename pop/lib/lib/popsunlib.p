/*  --- Copyright University of Sussex 1991.  All rights reserved. ---------
 > File:            C.all/lib/lib/popsunlib.p
 > Purpose:         Add the SUN library to -popautolist- and -popuseslist-
 > Author:          Ben Rubinstein, Nov 18 1986 (see revisions)
 > Documentation:
 > Related Files:   LIB * EXTEND_SEARCHLIST
 */
#_TERMIN_IF DEF POPC_COMPILING

section;

global vars popsunlib = ('$popsunlib/' dir_>< '');

extend_searchlist(popsunlib, popautolist, true) -> popautolist;
extend_searchlist(popsunlib, popuseslist, true) -> popuseslist;

endsection;


/* --- Revision History ---------------------------------------------------
--- Andreas Schoter, Sep  9 1991
    Changed occurrances of -popliblist- to -popautolist-
--- John Williams, Sep 14 1990
        Rewritten using -extend_searchlist-
--- James Goodlet, Jun  8 1989
        Removed trailing '/' from popsunlib.
 */
