/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XGraphicsContext.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;
uses XCoords;

global constant macro (

/*****************************************************************
 * GRAPHICS DEFINITIONS
 *****************************************************************/

    /* graphics functions, as in GC.alu */

    GXclear         = 16:0,     /* 0 */
    GXand           = 16:1,     /* src AND dst */
    GXandReverse    = 16:2,     /* src AND NOT dst */
    GXcopy          = 16:3,     /* src */
    GXandInverted   = 16:4,     /* NOT src AND dst */
    GXnoop          = 16:5,     /* dst */
    GXxor           = 16:6,     /* src XOR dst */
    GXor            = 16:7,     /* src OR dst */
    GXnor           = 16:8,     /* NOT src AND NOT dst */
    GXequiv         = 16:9,     /* NOT src XOR dst */
    GXinvert        = 16:A,     /* NOT dst */
    GXorReverse     = 16:B,     /* src OR NOT dst */
    GXcopyInverted  = 16:C,     /* NOT src */
    GXorInverted    = 16:D,     /* NOT src OR dst */
    GXnand          = 16:E,     /* NOT src OR NOT dst */
    GXset           = 16:F,     /* 1 */

    /* LineStyle */

    LineSolid       = 0,
    LineOnOffDash   = 1,
    LineDoubleDash  = 2,

    /* capStyle */

    CapNotLast      = 0,
    CapButt         = 1,
    CapRound        = 2,
    CapProjecting   = 3,

    /* joinStyle */

    JoinMiter       = 0,
    JoinRound       = 1,
    JoinBevel       = 2,

    /* fillStyle */

    FillSolid           = 0,
    FillTiled           = 1,
    FillStippled        = 2,
    FillOpaqueStippled  = 3,

    /* subwindow mode */

    ClipByChildren      = 0,
    IncludeInferiors    = 1,

    /* SetClipRectangles ordering */

    Unsorted        = 0,
    YSorted         = 1,
    YXSorted        = 2,
    YXBanded        = 3,

    /* GC components: masks used in CreateGC, CopyGC, ChangeGC, OR'ed into
       GC.stateChanges */

    GCFunction              = 1 << 0,
    GCPlaneMask             = 1 << 1,
    GCForeground            = 1 << 2,
    GCBackground            = 1 << 3,
    GCLineWidth             = 1 << 4,
    GCLineStyle             = 1 << 5,
    GCCapStyle              = 1 << 6,
    GCJoinStyle             = 1 << 7,
    GCFillStyle             = 1 << 8,
    GCFillRule              = 1 << 9,
    GCTile                  = 1 << 10,
    GCStipple               = 1 << 11,
    GCTileStipXOrigin       = 1 << 12,
    GCTileStipYOrigin       = 1 << 13,
    GCFont                  = 1 << 14,
    GCSubwindowMode         = 1 << 15,
    GCGraphicsExposures     = 1 << 16,
    GCClipXOrigin           = 1 << 17,
    GCClipYOrigin           = 1 << 18,
    GCClipMask              = 1 << 19,
    GCDashOffset            = 1 << 20,
    GCDashList              = 1 << 21,
    GCArcMode               = 1 << 22,

    GCLastBit               = 22,
);

external declare XGraphicsContext in c;
    (external_import_procedure XptImportProcedure)

    GContext XGContextFromGC(gc)
    GC gc;
    {}

    GC XCreateGC(display, drawable, valuemask, values)
    Display *display;
    Drawable drawable;
    unsigned long valuemask;
    XGCValues *values;
    {}

    void XChangeGC(display, gc, valuemask, values)
    Display *display;
    GC gc;
    unsigned long valuemask;
    XGCValues *values;
    {}

    void XCopyGC(display, src, valuemask, dest)
    Display *display;
    GC src, dest;
    unsigned long valuemask;
    {}

    void XFreeGC(display, gc)
    Display *display;
    GC gc;
    {}

    void XSetArcMode(display, gc, arc_mode)
    Display *display;
    GC gc;
    int arc_mode;
    {}

    void XSetClipMask(display, gc, clip_mask)
    Display *display;
    GC gc;
    Pixmap clip_mask;
    {}

    void XSetClipOrigin(display, gc, clip_x_origin, clip_y_origin)
    Display *display;
    GC gc;
    int clip_x_origin, clip_y_origin;
    {}

    void XSetClipRectangles(display, gc, clip_x_origin, clip_y_origin, rectangles,
                                nrects, ordering)
    Display *display;
    GC gc;
    int clip_x_origin, clip_y_origin;
    XRectangle rectangles[];
    int nrects;
    int ordering;
    {}

    void XSetRegion(display, gc, r)
    Display *display;
    GC gc;
    Region r;
    {}

    void XSetDashes(display, gc, dash_offset, dash_list, n)
    Display *display;
    GC gc;
    int dash_offset;
    char dash_list[];
    int n;
    {}

    void XSetLineAttributes(display, gc, line_width, line_style, cap_style,
                              join_style)
    Display *display;
    GC gc;
    unsigned int line_width;
    int line_style;
    int cap_style;
    int join_style;
    {}

    void XSetFillRule(display, gc, fill_rule)
    Display *display;
    GC gc;
    int fill_rule;
    {}

    void XSetFillStyle(display, gc, fill_style)
    Display *display;
    GC gc;
    int fill_style;
    {}

    ;;; duplicated from tiles
    void XSetTile(display, gc, tile)
    Display *display;
    GC gc;
    Pixmap tile;
    {}

    void XSetStipple(display, gc, stipple)
    Display *display;
    GC gc;
    Pixmap stipple;
    {}

    void XSetTSOrigin(display, gc, ts_x_origin, ts_y_origin)
    Display *display;
    GC gc;
    int ts_x_origin, ts_y_origin;
    {}

    void XSetGraphicsExposures(display, gc, graphics_exposures)
    Display *display;
    GC gc;
    Bool graphics_exposures;
    {}

    void XSetForeground(display, gc, foreground)
    Display *display;
    GC gc;
    unsigned long foreground;
    {}

    void XSetBackground(display, gc, background)
    Display *display;
    GC gc;
    unsigned long background;
    {}

    void XSetFunction(display, gc, function)
    Display *display;
    GC gc;
    int function;
    {}

    void XSetPlaneMask(display, gc, plane_mask)
    Display *display;
    GC gc;
    unsigned long plane_mask;
    {}

    void XSetState(display, gc, foreground, background, function, plane_mask)
    Display *display;
    GC gc;
    unsigned long foreground, background;
    int function;
    unsigned long plane_mask;
    {}

    void XSetSubwindowMode(display, gc, subwindow_mode)
    Display *display;
    GC gc;
    int subwindow_mode;
    {}

    Status XGetGCValues(display, gc, valuemask, values_return)
    Display *display;
    GC gc;
    unsigned long valuemask;
    XGCValues *values_return;
    {}

endexternal;


xlib_external_require XGraphicsContext;


;;; duplicated from XlibMacros
 ;;; #define DefaultGC(dpy, scr)     (((dpy)->screens[(scr)]).default_gc)
define global DefaultGC(dpy, scr);
    lvars dpy scr;
    (dpy #-> screens)(scr) #: default_gc
enddefine;


global vars XGraphicsContext = true;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jan 24 1992 : Added -XGetGCValues-
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
