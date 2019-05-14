/* --- Copyright University of Sussex 1991.  All rights reserved. ---------
 > File:           C.all/lib/lib/flavours.p
 > Purpose:        load the flavours package.
 > Author:         Mark Rubinstein, Apr 17 1986 (see revisions)
 > Documentation:  TEACH * FLAVOURS
 > Related Files:  file in $usepop/pop/lib/flavours
 */

#_TERMIN_IF DEF POPC_COMPILING


;;; --- ADD THE FLAVOUR LIBRARY TO THE LIBRARY LISTS -----------------------

section;

global vars flavours;

unless isundef(flavours) do
    [endsection;] -> proglist
endunless;

lconstant flavourlibrary = '$usepop/pop/lib/flavours' dir_>< '';

extend_searchlist(flavourlibrary, popautolist) -> popautolist;
extend_searchlist(flavourlibrary, popuseslist) -> popuseslist;

pop_optimise;
true -> pop_optimise;

;;; the core base for the flavours system
pop11_compile(flavourlibrary dir_>< 'flavecore.p');
;;; the syntax
pop11_compile(flavourlibrary dir_>< 'flavesyntax.p');
;;; the initial flavour declarations.
pop11_compile(flavourlibrary dir_>< 'baseflavours.p');

-> pop_optimise;

true -> flavours;   ;;; for -uses-

endsection;

/* --- Revision History ---------------------------------------------------
--- Ian Rogers, Dec 18 1991
    Changed to use -extend_searchlist-
--- Andreas Schoter, Sep  9 1991
    Changed occurrances of -popliblist- to -popautolist-
--- John Williams, Oct 28 1987 made towork with -uses- (cf BR johnw.97)
 */
