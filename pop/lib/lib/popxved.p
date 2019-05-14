/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lib/popxved.p
 > Purpose:         Set up lib lists etc. for access to X ved
 > Author:          Jonathan Meyer, Apr  1 1991 (see revisions)
 > Documentation:   HELP *XVED
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

uses popxlib;

include xveddefs.ph;

/* Code lists */
;;; Make the additions go at the end
declare_incremental list[prec=300] (popautolist, popuseslist, popsyslist);

extend_searchlist(XVEDAUTO, popautolist, true) -> popautolist;
extend_searchlist(XVEDLIB, popuseslist, true) -> popuseslist;
extend_searchlist(XVEDSRC, popsyslist, true) -> popsyslist;

/* Documentation lists */

#_IF isdefined("vedprocess") or DEF POPC_COMPILING

uses ved_src;       ;;; for vedsrclist

lconstant
    Xvedhelp = [^XVEDHELP help],
    Xvedref = [^XVEDREF ref],
    Xvedteach = [^XVEDTEACH teach],
    Xvedsrc = [^XVEDSRC src],

    Xvedhelplist    =   [^Xvedhelp ^Xvedref ^Xvedteach],
    Xvedreflist     =   [^Xvedref ^Xvedhelp ^Xvedteach],
    Xvedteachlist   =   [^Xvedteach ^Xvedhelp ^Xvedref],
    Xvedsrclist     =   [^Xvedsrc],
;

extend_searchlist(Xvedhelplist, weakref vedhelplist)  -> weakref vedhelplist;
extend_searchlist(Xvedreflist, weakref vedreflist)   -> weakref vedreflist;
extend_searchlist(Xvedteachlist, weakref vedteachlist) -> weakref vedteachlist;
extend_searchlist(Xvedsrclist, weakref vedsrclist) -> weakref vedsrclist;

#_ENDIF

constant popxved = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 28 1993
        Removed addition of XVEDI*NCLUDE to popincludelist  -- now
        just uses standard X include dir.
--- John Gibson, Jun  8 1993
        Adds XVEDSRC to popsyslist
--- John Gibson, May 24 1993
        Removed addition of XVEDLIB to popautolist (things that are
        intended to be autoloadable should be in XVEDAUTO!!)
--- Andreas Schoter, Sep  9 1991
    Changed occurrances of -popliblist- to -popautolist-
--- Jonathan Meyer, Jun  3 1991 Applied change as with popxlib.p:
        --- Roger Evans, Jun  3 1991 changed the vedhelplist (etc). entries to
        be the value of Xpophelplist (etc) instead of the ident (pretty
        useless if they're lconstants, and confuses extend searchlist on
        multiple loading - bugsee davidy.36

 */
