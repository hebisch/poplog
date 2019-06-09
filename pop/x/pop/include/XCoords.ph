/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/include/XCoords.ph
 > Purpose:         Provide declarations for LIB XCoords
 > Author:          Ian Rogers, Aug 18 1989 (see revisions)
 > Documentation:
 > Related Files:   LIB * XGraphicsContext * XDrawingPrimitives * XRegions
 */

#_TERMIN_IF DEF XCOORDS_INCLUDED

section;

iconstant macro(

    /* fillRule */

    EvenOddRule     = 0,
    WindingRule     = 1,
);

iconstant XCOORDS_INCLUDED = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991
        moved from x/pop/lib/xlib to x/pop/include.
        Made it use iconstant
 */
