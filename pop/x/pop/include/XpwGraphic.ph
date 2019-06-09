/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/include/XpwGraphic.ph
 > Purpose:         Provide macros required for lib XpwGraphic
 > Author:          Aaron Sloman, May 21 1990 (see revisions)
 > Documentation:   See HELP * XpwGraphic, REF * XpwGraphic
 > Related Files:   LIB * XpwGraphic
 */

#_TERMIN_IF DEF XPWGRAPHIC_INCLUDED

section;

iconstant macro (
        XpwMAllocColorRange = 400,
        XpwMFreeColorRange  = 406,
        XpwMAllocStoreColor = 401,
        XpwMSetPixelColor   = 402,
        XpwMCreateColormap  = 403,
        XpwMFreeColormap    = 404,
        XpwMFreeColors      = 404,
);

iconstant XPWGRAPHIC_INCLUDED = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 29 1991
            Removed redundant XtN declarations
            Reinstated iconstant
--- Roger Evans, Oct 11 1990 moved to include directory
--- Andreas Schoter, July 16 1990
    Renamed to XpwGraphic.ph.p and changed all variable names from Pop* to
    Xpw*
--- Aaron Sloman, May 23 1990
    Replaced iconstant with lvars
--- Aaron Sloman, May 23 1990
    Separated these declarations from the .p file, and changed to use
    iconstant.
 */
