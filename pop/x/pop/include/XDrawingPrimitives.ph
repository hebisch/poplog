/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/include/XDrawingPrimitives.ph
 > Purpose:         Provide declarations for XDrawingPrimitives.p
 > Author:          Aaron Sloman 23 May 1990 (see revisions)
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF XDRAWINGPRIMITIVES_INCLUDED

section;

iconstant macro (

    /* CoordinateMode for drawing routines */

    CoordModeOrigin     = 0,    /* relative to the origin */
    CoordModePrevious   = 1,    /* relative to previous point */

    /* Polygon shapes */

    Complex         = 0,    /* paths may intersect */
    Nonconvex       = 1,    /* no paths intersect, but not convex */
    Convex          = 2,    /* wholly convex */

    /* Arc modes for PolyFillArc */

    ArcChord        = 0,    /* join endpoints of arc */
    ArcPieSlice     = 1,    /* join endpoints to center of arc */

    ;;; Vertex flags, to determine plotting mode used in -XDraw-
    /* The meanings of the flag bits.  If the bit is 1 the predicate is true */
    VertexRelative          = 16:0001,  ;;; else absolute
    VertexDontDraw          = 16:0002,  ;;; else draw
    VertexCurved            = 16:0004,  ;;; else straight
    VertexStartClosed       = 16:0008,  ;;; else not
    VertexEndClosed         = 16:0010,  ;;; else not
    ;;; this one isn't handled
    ;;; VertexDrawLastPoint     = 16:0020,  ;;; else don't

    );

iconstant XDRAWINGPRIMITIVES_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991
        moved from x/pop/lib/xlib to x/pop/include.
        Made it use iconstant

 */
