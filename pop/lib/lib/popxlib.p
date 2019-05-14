/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lib/popxlib.p
 > Purpose:         Set up lib lists etc. for access to X libraries
 > Author:          Roger Evans, Jun 15 1989 (see revisions)
 > Documentation:   HELP * XLIB, HELP * X
 > Related Files:   LIB * XLIB,
 */
compile_mode :pop11 +strict;

include xdefs.ph

section;

/* Set a non-standard popexlinkbase if specified in header file */

if XPOPSTB then
    sysfileok(XPOPSTB, false) -> systranslate('popexlinkbase');
endif;


/* Set up library search lists */

;;; Make the additions go at the end
declare_incremental list[prec=100]
    (popautolist, popuseslist, popsyslist, popincludelist);

extend_searchlist(XPOPAUTO, popautolist, true) -> popautolist;
extend_searchlist(XPOPLIB, popuseslist, true) -> popuseslist;
extend_searchlist(XPOPSRC, popsyslist, true) -> popsyslist;
extend_searchlist(XPOPINCLUDE, popincludelist, true) -> popincludelist;


/* Set up documentation search lists */

#_IF DEF vedprocess or DEF POPC_COMPILING

#_IF not(DEF POPC_COMPILING)
uses ved_src;       ;;; for vedsrclist
#_ENDIF

lconstant
    Xpopdoc         =   [^XPOPDOC doc],
    Xpophelp        =   [^XPOPHELP help],
    Xpopref         =   [^XPOPREF ref],
    Xpopsrc         =   [^XPOPSRC src],
    Xpopxpw         =   [^XPOPXPW src],
    Xpopteach       =   [^XPOPTEACH teach],
    Xpopdoclist     =   [^Xpopdoc ^Xpophelp ^Xpopref ^Xpopteach],
    Xpophelplist    =   [^Xpophelp ^Xpopref ^Xpopteach ^Xpopdoc],
    Xpopreflist     =   [^Xpopref ^Xpophelp ^Xpopteach ^Xpopdoc],
    Xpopsrclist     =   [^Xpopsrc ^Xpopxpw],
    Xpopteachlist   =   [^Xpopteach ^Xpophelp ^Xpopref ^Xpopdoc],
    ;

extend_searchlist(Xpopdoclist, weakref veddoclist) -> weakref veddoclist;
extend_searchlist(Xpophelplist, weakref vedhelplist) -> weakref vedhelplist;
extend_searchlist(Xpopreflist, weakref vedreflist) -> weakref vedreflist;
extend_searchlist(Xpopsrclist, weakref vedsrclist) -> weakref vedsrclist;
extend_searchlist(Xpopteachlist, weakref vedteachlist) -> weakref vedteachlist;

#_ENDIF


constant popxlib = true;    ;;; For uses

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun  8 1993
        Adds XPOPSRC to popsyslist
--- John Gibson, Oct 29 1992
        Removed XPOPLIB from popautolist -- autoloadable names are supposed
        to be in XPOPAUTO.
--- Robert John Duncan, May 15 1992
        Made compileable in a system without VED.
--- Andreas Schoter, Sep  9 1991
        Changed occurrances of -popliblist- to -popautolist-
--- Ian Rogers, Aug 15 1991
        All lib directories are now at the end of their search lists.
        cf. BR ianr.17
--- Jonathan Meyer, Jul 30 1991
        Addded Xpw C sources to Xpopsrc
--- Roger Evans, Jun  3 1991 changed the vedhelplist (etc). entries to
        be the value of Xpophelplist (etc) instead of the ident (pretty
        useless if they're lconstants, and confuses extend searchlist on
        multiple loading - bugsee davidy.36
--- Ian Rogers, Feb 22 1991
        Removed XPOPLIB from the -include- search directory, and put
        XPOPINCLUDE at the *end* cf. BR rogere.43
--- John Williams, Dec  3 1990
        Sets up documentation search lists properly.
--- Aaron Sloman, Oct 29 1990
        Made each documentation search list include all the other directories
--- Roger Evans, Oct 17 1990
        Added XPOPAUTO and new popexlinkbase support
--- Roger Evans, Oct 16 1990
        Removed references to $Xroot
--- Roger Evans, Oct 16 1990
        Removed XptSetExLinkBase
--- Simon Nichols, Oct  5 1990
        Removed references to $Xpop and $Xpopbin.
--- John Williams, Sep 14 1990
        Removed 'uses extend_searchlist'
--- Roger Evans, Aug 24 1990
        Added XptSetExLinkBase, XPOPINCLUDE, and changed to use
        updater of systranslate
--- Aaron Sloman, Jul 28 1990
        Replaced extendsearchlist with extend_searchlist, and added
        line for veddoclist
--- Roger Evans, Jun  1 1990
        Changed add_if_necessary to extendsearchlist - a new library
--- Roger Evans, May 31 1990
        Put constants into xdefs.ph, and added explicit code for
        checking link base
 */
