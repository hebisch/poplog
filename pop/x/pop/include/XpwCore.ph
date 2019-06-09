/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/include/XpwCore.ph
 > Purpose:         Macros required for Lib XpwCore
 > Author:          J. Meyer Jan 1990 (see revisions)
 > Documentation:   HELP * XpwCore
 > Related Files:   LIB * XpwCore
 */

#_TERMIN_IF DEF XPWCORE_INCLUDED

section;

iconstant macro (

/* methods */

        XpwMSetFont          = 100,
        XpwMFreeFont         = 102,
        XpwMSetCursor        = 101,
        XpwMFreeCursor       = 103,
        XpwMSetColor         = 104,
        XpwMFreeColor        = 105,
        XpwMLoadPixmap       = 106,
        XpwMChangeUsersGC    = 107,
);

iconstant XPWCORE_INCLUDED = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Oct  4 1991
        Added XpwMChangeUsersGC
--- Jonathan Meyer, Jul 29 1991
        Added XpwMLoadPixmap
--- Jonathan Meyer, Jan 29 1991
        Removed redundant XtN* declarations. Reinstated iconstant
--- Roger Evans, Oct 11 1990 moved to include directory
--- Andreas Schoter, Jul 23 1990
    Changed all pop* identifiers to xpw*
--- Andreas Schoter, July 16 1990
    Renamed to XpwCore.ph and changed all variable names from Pop* to
    Xpw*
--- Aaron Sloman, May 23 1990
    Replaced iconstant with lvars
--- Aaron Sloman, May 23 1990
    Separated these declarations from the .p file, and changed to use
    iconstant.
 */
