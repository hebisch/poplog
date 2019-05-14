/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/include/xdefs.ph
 > Purpose:         Common public definitions for X libraries
 > Author:          Roger Evans, May 31 1990 (see revisions)
 > Documentation:   REF * X
 > Related Files:   LIB * POPXLIB
 */

#_TERMIN_IF DEF XDEFS_INCLUDED

section;

iconstant
    ;;; root of xpop subtree
    XPOP        = '$usepop/pop' dir_>< 'x/',

    ;;; popexlinkbase value (set by lib popxlib if non-false)
    XPOPSTB     = false,

    ;;; standard xpop subdirectories
    XPOPAUTO    = XPOP dir_>< 'pop/auto/',
    XPOPLIB     = XPOP dir_>< 'pop/lib/',
    XPOPXLIB    = XPOPLIB dir_>< 'xlib/',
    XPOPINCLUDE = XPOP dir_>< 'pop/include/',
    XPOPHELP    = XPOP dir_>< 'pop/help/',
    XPOPTEACH   = XPOP dir_>< 'pop/teach/',
    XPOPDOC     = XPOP dir_>< 'pop/doc/',
    XPOPREF     = XPOP dir_>< 'pop/ref/',
    XPOPSRC     = XPOP dir_>< 'src/',
    XPOPDEMO    = XPOP dir_>< 'pop/demo/',
    XPOPXPW     = XPOP dir_>< 'Xpw/',
;


    ;;; rest are for backward compatibility
iconstant
    ;;; X implementation
    XOPENLOOK   = DEF popxlink_openlook,
    XMOTIF      = DEF popxlink_motif,
    XMIT        = DEF popxlink_mit,
;

iconstant XDEFS_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 13 1993
        Removed XPW*LIBFILE -- no longer possible to define it as a single
        string (since XPW_EXLIBS now contains -L$popexternlib -lXpw)
--- John Gibson, May 10 1993
        Add DEF popxlink_* for old XOPENLOOK etc
--- John Gibson, May  4 1993
        Put back a definition of XPWLIBFILE for backward compatibility
--- John Gibson, Mar 27 1993
        Moved out all library and dir lists to be autoloadable vars in
        x/pop/auto. (Setting of XLINK_LIBS to nil if XLINK_COMPLETE is true
        is now done directly by pglink.)
--- John Gibson, Mar 15 1993
        Added #_IF for POPC_COMPILING
--- John Gibson, Oct 29 1992
        Added #_TERMIN_IF
--- James Goodlet, Mar 28 1992
        Made the assignment of XPWLIBFILE conditional on it not already
        being defined.
--- Adrian Howard, Feb 20 1992 : Corrected default value for -XLINK_VERSION-
--- Adrian Howard, Feb 18 1992 : Added -XLINK_VERSION- for support of
        different versions of toolkits/widget sets
--- John Gibson, Oct 25 1991
        Added VMS definition for XPWLIBFILE
--- Jonathan Meyer, Jul 31 1991 Added XPWLIBFILE
--- Jonathan Meyer, Jul 31 1991 XPOPBIN no longer exists
--- Jonathan Meyer, Jul 30 1991 Added XPOPXPW
--- Robert John Duncan, Jun 27 1991
        Removed XPOP*COMPAT
--- Robert John Duncan, Jun 11 1991
        Thorough revision based on XLINK_* names
--- Robert John Duncan, Mar 15 1991
        Added definitions for DECstation MOTIF
--- Roger Evans, Feb 26 1991
        changed default XHASOLDX to false - more robust not to assume oldX
--- John Gibson, Feb 14 1991
        Added VMS mods
--- Jonathan Meyer, Jan 25 1991 added XHASOLDX and changed XTLIBDIRS
--- Jonathan Meyer, Jan 17 1991 added XPOPDEMO
--- Roger Evans, Oct 17 1990 added XPOPAUTO XDEFS_LOADED XTLIBDIRS/FILES
--- Roger Evans, Oct 16 1990 added XTBASELIBS. XOPENWINDOWS, XMIT
--- Simon Nichols, Oct  5 1990
        Removed references to $Xpop and tidied up.
--- Roger Evans, Aug 24 1990 added XPOPINCLUDE
--- Aaron Sloman, Jul 28 1990 added XPOPDOC
 */
