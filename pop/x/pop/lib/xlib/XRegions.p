/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XRegions.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */



uses XConstants;
uses XCoords;

global constant macro (

    /* Return values from XRectInRegion() */

    RectangleOut  = 0,
    RectangleIn   = 1,
    RectanglePart = 2,

);


external declare XRegions in c;
    (external_import_procedure XptImportProcedure)

    Region XCreateRegion()
    {}

    void XDestroyRegion(r)
    Region r;
    {}

    int XEmptyRegion(r)
    Region r;
    {}

    Region XPolygonRegion(points, n, fill_rule)
    XPoint points[];
    int n;
    int fill_rule;
    {}

    int XPointInRegion(r, x, y)
    Region r;
    int x, y;
    {}

    int XRectInRegion(r, x, y, width, height)
    Region r;
    int x, y;
    unsigned int width, height;
    {}

    void XUnionRectWithRegion(rectangle, src_region, dest_region)
    XRectangle *rectangle;
    Region src_region;
    Region dest_region;
    {}

    void XClipBox(r, rect)
    Region r;
    XRectangle *rect;           ;;; RETURN
    {}

    void XOffsetRegion(r, dx, dy)
    Region r;
    int dx, dy;
    {}

    void XShrinkRegion(r, dx, dy)
    Region r;
    int dx, dy;
    {}

    int XEqualRegion(r1, r2)
    Region r1, r2;
    {}

    void XSetRegion(display, gc, r)
    Display *display;
    GC gc;
    Region r;
    {}

    void XSubtractRegion(sra, srb, dr)
    Region sra, srb;
    Region dr;              ;;; RETURN
    {}

    void XIntersectRegion(sra, srb, dr)
    Region sra, srb;
    Region dr;              ;;; RETURN
    {}

    void XUnionRegion(sra, srb, dr)
    Region sra, srb;
    Region dr;
    {}

    void XXorRegion(sra, srb, dr)
    Region sra, srb;
    Region dr;              ;;; RETURN
    {}

endexternal;


xlib_external_require XRegions;


global vars XRegions = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
