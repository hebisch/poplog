/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/lib/popxdemo.p
 > Purpose:         Set up lib lists etc. for access to X demo libraries
 > Author:          Jonathan Meyer, Jan 27 1991 (see revisions)
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

uses popxlib;
include xdefs.ph

extend_searchlist(XPOPDEMO, popautolist) -> popautolist;
extend_searchlist(XPOPDEMO, popuseslist) -> popuseslist;

global constant popxdemo = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Andreas Schoter, Sep  9 1991
    Changed occurrances of -popliblist- to -popautolist-
--- Jonathan Meyer, Jan 29 1991 added global constant popxdemo
 */
