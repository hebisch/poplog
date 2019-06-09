/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/include/XpwPixmap.ph
 > Purpose:         Provide macros for facilities in lib XpwPixmap
 > Author:          Aaron Sloman, May 21 1990 (see revisions)
 > Documentation:   HELP * XpwGraphic, * Xpw, REF * XpwPixmap
 > Related Files:   LIB * XpwPixmap
 */

#_TERMIN_IF DEF XPWPIXMAP_INCLUDED

section;

iconstant macro (

/* function modes */
        GXclear          = 0,
        GXand            = 1,
        GXandReverse     = 2,
        GXcopy           = 3,
        GXandInverted    = 4,
        GXnoop           = 5,
        GXxor            = 6,
        GXor             = 7,
        GXnor            = 8,
        GXequiv          = 9,
        GXinvert         = 10,
        GXorReverse      = 11,
        GXcopyInverted   = 12,
        GXorInverted     = 13,
        GXnand           = 14,
        GXset            = 15,

/* cap style */

        CapNotLast          = 0,
        CapButt             = 1,
        CapRound            = 2,
        CapProjecting       = 3,

/* join style */

        JoinMiter           = 0,
        JoinRound           = 1,
        JoinBevel           = 2,

/* line style */

        LineSolid           = 0,
        LineOnOffDash       = 1,
        LineDoubleDash      = 2,

/* subwindow mode */

        ClipByChildren      = 0,
        IncludeInferiors    = 1,

/* fill style */

        FillSolid           = 0,
        FillTiled           = 1,
        FillStippled        = 2,
        FillOpaqueStippled  = 3,

/* fill rule */

        EvenOddRule         = 0,
        WindingRule         = 1,

 /* drawing mode */

        CoordModeOrigin     = 0,
        CoordModePrevious   = 1,

/* image types */

        XYPixmap            = 1,
        ZPixmap             = 2,

/* resources */

        PixmapOn            = 0,
        PixmapOff           = 1,
        PixmapHasNone       = 2,
        PixmapOnly          = 3,

/* arc mode */

        ArcChord            = 0,
        ArcPieSlice         = 1,

/* fill algorithm */
        Complex             = 0,
        Nonconvex           = 1,
        Convex              = 2,

);

#_IF not(DEF AllPlanes)
/* clip mask (this is duplicated by xlib XlibMacros) */
iconstant macro AllPlanes = -1;
#_ENDIF

iconstant macro (

/* poplog methods */

        XpwMDraw                = 200,
        XpwMDrawFilled      = 201,
        XpwMDrawPoint       = 202,
        XpwMDrawPoints      = 210,
        XpwMDrawLine            = 203,
        XpwMDrawLines       = 211,
        XpwMDrawArc         = 204,
        XpwMDrawArcs        = 212,
        XpwMDrawRectangle   = 205,
        XpwMDrawRectangles  = 213,
        XpwMDrawSegments     = 214,
        XpwMDrawString   = 206,
        XpwMDrawImageString  = 207,
        XpwMFillArc  = 215,
        XpwMFillArcs     = 216,
        XpwMFillPolygon  = 217,
        XpwMFillRectangle    = 218,
        XpwMFillRectangles   = 219,
        XpwMClearArea    = 208,
        XpwMClearWindow  = 209,
        XpwMCopyFrom     = 220,
        XpwMCopyTo       = 221,
        XpwMPutImage     = 222,
        XpwMGetImage     = 223,
        XpwMDrawImage    = 224,
        XpwMCreateImage  = 226,
        XpwMDrawRoundedRectangle = 228,
        XpwMFillRoundedRectangle = 229,
);

iconstant XPWPIXMAP_INCLUDED = true;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 31 1995
        Added #_IF around AllPlanes
--- Adrian Howard, Apr  3 1992
        Put back the old global constant declarations. Making them all
        INCLUDE_constant makes Xpw & rc_graphic non-upward compatable.
--- Ian Rogers, Dec 18 1991
        *All* identifiers declared by INCLUDE_constant
--- Jonathan Meyer, Sep 25 1991 Added polygon fill shapes
--- Jonathan Meyer, Aug  1 1991 Added AllPlanes
--- Jason Handby, Jul 31 1991 Added ArcChord and ArcPieSlice
--- Jonathan Meyer, Jul 30 1991 Added CreateImage
--- Jonathan Meyer, Jul 29 1991 Added fill style and fill rule
--- Jonathan Meyer, Jul  2 1991 Added RoundedRectangle
--- Jonathan Meyer, Jan 29 1991 Reinstated INCLUDE_constant
--- Jonathan Meyer, Jan 17 1991 Removed XtN* string definition macros
--- Roger Evans, Oct 11 1990 moved to include directory
--- Jonathan Meyer, Jul 24 1990
    Added PixmapOnly = 3
--- Andreas Schoter, July 16 1990
    Renamed to XpwPixmap.ph and changed all variable names from Pop* to
    Xpw*
--- Aaron Sloman, May 23 1990
    Replaced INCLUDE_constant with lvars
--- Aaron Sloman, May 23 1990
    Separated these declarations from the .p file, and changed to use
    INCLUDE_constant. Should they all use it?
 */
