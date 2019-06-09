/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XDrawingPrimitives.p
 > Purpose:
 > Author:          Ian Rogers, Gareth Palmer  3 July 1989 (see revisions)
 > Documentation:
 > Related Files:   LIB * XDrawingPrimitives.ph
 */

section;

uses XConstants;
uses XCoords;
include xdefs.ph;

loadinclude XDrawingPrimitives.ph;

external declare XDrawingPrimitives in c;
    (external_import_procedure XptImportProcedure)


    typedef struct {
        short x, y;
        unsigned short width, height;
        short angle1, angle2;
    } XArc;

    typedef struct {
        short x1, y1, x2, y2;
    } XSegment;

    /* Used in XDraw and XDrawFilled */

    typedef struct {
        short x, y;
        unsigned short flags;
    } Vertex;


#_IF DEF XHASOLDX

    Status XDraw(display, drawable, gc, vlist, vcount)
    Display *display;
    Drawable drawable;
    GC gc;
    Vertex *vlist;
    int vcount;
    {}

    Status XDrawFilled(display, drawable, gc, vlist, vcount)
    Display *display;
    Drawable drawable;
    GC gc;
    Vertex *vlist;
    int vcount;
    {}

#_ENDIF

    void XDrawArc(display, drawable, gc, x, y, width, height, angle1, angle2)
    Display *display;
    Drawable drawable;
    GC gc;
    int x, y;
    unsigned int width, height;
    int angle1, angle2;
    {}

    void XDrawArcs(display, drawable, gc, arcs, narcs)
    Display *display;
    Drawable drawable;
    GC gc;
    XArc *arcs;
    int narcs;
    {}


    void XDrawLine(display, drawable, gc, x1, y1, x2, y2)
    Display *display;
    Drawable drawable;
    GC gc;
    int x1, y1, x2, y2;
    {}

    void XDrawLines(display, drawable, gc, points, npoints, mode)
    Display *display;
    Drawable drawable;
    GC gc;
    XPoint *points;
    int npoints;
    int mode;
    {}

    void XDrawPoint(display, drawable, gc, x, y)
    Display *display;
    Drawable drawable;
    GC gc;
    int x, y;
    {}

    void XDrawPoints(display, drawable, gc, points, npoints, mode)
    Display *display;
    Drawable drawable;
    GC gc;
    XPoint *points;
    int npoints;
    int mode;
    {}

    void XDrawRectangle(display, drawable, gc, x, y, width, height)
    Display *display;
    Drawable drawable;
    GC gc;
    int x, y;
    unsigned int width, height;
    {}

    void XDrawRectangles(display, drawable, gc, rectangles, nrectangles)
    Display *display;
    Drawable drawable;
    GC gc;
    XRectangle rectangles[];
    int nrectangles;
    {}

    void XDrawSegments(display, drawable, gc, segments, nsegments)
    Display *display;
    Drawable drawable;
    GC gc;
    XSegment *segments;
    int nsegments;
    {}

    void XCopyArea(display, src, dest, gc, src_x, src_y, width, height,
                    dest_x, dest_y)
    Display *display;
    Drawable src, dest;
    GC gc;
    int src_x, src_y;
    unsigned int width, height;
    int dest_x, dest_y;
    {}

    void XCopyPlane(display, src, dest, gc, src_x, src_y, width, height,
                    dest_x, dest_y, plane)
    Display *display;
    Drawable src, dest;
    GC gc;
    int src_x, src_y;
    unsigned int width, height;
    int dest_x, dest_y;
    unsigned long plane;
    {}

    void XFillArc(display, drawable, gc, x, y, width, height, angle1, angle2)
    Display *display;
    Drawable drawable;
    GC gc;
    int x, y;
    unsigned int width, height;
    int angle1, angle2;
    {}

    void XFillArcs(display, drawable, gc, arcs, narcs)
    Display *display;
    Drawable drawable;
    GC gc;
    XArc *arcs;
    int narcs;
    {}

    void XFillPolygon(display, drawable, gc, points, npoints, shape, mode)
    Display *display;
    Drawable drawable;
    GC gc;
    XPoint *points;
    int npoints;
    int shape;
    int mode;
    {}

    void XFillRectangle(display, drawable, gc, x, y, width, height)
    Display *display;
    Drawable drawable;
    GC gc;
    int x, y;
    unsigned int width, height;
    {}

    void XFillRectangles(display, drawable, gc, rectangles, nrectangles)
    Display *display;
    Drawable drawable;
    GC gc;
    XRectangle *rectangles;
    int nrectangles;
    {}

    void XClearArea(display, w, x, y, width, height, exposures)
    Display *display;
    Window w;
    int x, y;
    unsigned int width, height;
    Bool exposures;
    {}

    void XClearWindow(display, w)
    Display *display;
    Window w;
    {}


endexternal;


xlib_external_require XDrawingPrimitives;

global vars XDrawingPrimitives = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Ian Rogers, Jul 12 1991
        Corrected test for XHASOLDX (added DEF)
--- Jonathan Meyer, Jan 25 1991
        Added test for XHASOLDX.
        Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
--- Aaron Sloman, May 23 1990
    Moved macro declarations to .ph file
 */
