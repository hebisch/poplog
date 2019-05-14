/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.all/lib/lib/pop_ui.p
 > Purpose:         Poplog Graphical User Interface loader
 > Author:          Julian Clinton, July 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

section;

uses popxlib;

lconstant
        POPUILIB    = '$usepop/pop/x/ui/lib/',
;

    /* Tell POPC the precedence of this within popautolist */
declare_incremental list[prec=400] popautolist;

extend_searchlist(POPUILIB, popautolist, true) -> popautolist;

constant pop_ui = true;  ;;; for uses

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, May 24 1993
        Removed unnecessary addtion to popuseslist
--- John Gibson, Oct 21 1992
        Added #_TERMIN_IF for POPC
--- Integral Solutions Ltd, Jun  2 1992 (Julian Clinton)
    Removed definitions for local UI help and ref dirs now the UI
    on-line docs have been moved.
--- Simon Nichols, Oct 23 1991
        Added trailing '/' to each directory name.
--- Andreas Schoter, Sep  9 1991
    Changed occurrances of -popliblist- to -popautolist-
--- Jonathan Meyer, Aug 29 1991
        defined pop_ui = true for uses
 */
