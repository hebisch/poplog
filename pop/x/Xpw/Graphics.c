/*--- Copyright University of Sussex 1997. All rights reserved. ----------
 * File:      C.x/x/Xpw/Graphics.c
 * Version:   Revision 5.0
 * Purpose:       Method definitions for XpwPixmap widgets.
 * Author:    Jonathan Meyer (additions James Goodlet) (see revisions)
 * Date:      11 Feb 1989, revised 10 April 1989, 26 Aug 1989
 * Documentation: HELP *Xpw, REF *XpwPixmap, REF *XpwGraphic,
 * Related files: XpwGraphic.c XpwPixmap.c GraphMethods.c
 */

/* using Xpw header files locally, not in <X11/Xpw/...> */
#define XpwNotInstalled

#include <X11/Xlib.h>
#include <X11/Xatom.h>      /* XA_COLORMAP definition */
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "XpwGraphicP.h"

#ifdef XtSpecificationRelease
#include <X11/ObjectP.h>
#endif

#ifdef VMS
#ifndef bcopy
#define bcopy(P1,P2,N) memcpy((char* )(P2), (char *)(P1), (N))
#endif
#endif

/****************************************************************
 *
 * XpwPixmap Method Table Declaration
 *
 ****************************************************************/

extern void _XpwSyntheticExposeEvent();

static XpwMethodRet /* methods with return values */
            CreateImage(),
            GetImage();

static void  /* methods private to pixmap widgets*/
            DrawRoundedRect(), FillRoundedRect(),
            CopyFrom(), CopyTo(),
            PutImage(), CreatePutImage(),
            ClearPixmap(), ClearPixmapArea();


/****************************************************************
 *
 *  XpwPixmap Method definitions
 *
 ****************************************************************/

#define getgc(w) (XtIsSubclass((Widget)w,xpwGraphicWidgetClass) ? \
            w->xpwgraphic.my_gc : w->xpwpixmap.private_gc)

static void ClearPixmap(w)
XpwGraphicWidget w;
{
    GC gc;
    Pixmap pm = w->xpwpixmap.pixmap;
    Display *dpy = XtDisplay(w);
    gc = getgc(w);
    XSetForeground(dpy,gc,w->core.background_pixel);
    XFillRectangle(dpy, pm, gc, 0,0, w->core.width, w->core.height);
    XSetForeground(dpy,gc,w->xpwcore.foreground_pixel);
}

static void ClearPixmapArea(w, x,y,width,height)
XpwPixmapWidget w;
int x,y,width,height;
{
    GC gc = w->xpwpixmap.private_gc;
    Pixmap pm = w->xpwpixmap.pixmap;
    Display *dpy = XtDisplay(w);
    if (width == 0) width = w->core.width;
    if (height == 0) height = w->core.height;
    XSetForeground(dpy,gc,w->core.background_pixel);
    XFillRectangle(dpy, pm, gc, x,y,width,height);
    XSetForeground(dpy,gc,w->xpwcore.foreground_pixel);
}

static void CopyFrom(w,dest_win,source_widget, x,y,dx,dy,ex,ey)
XpwGraphicWidget w;
Widget source_widget;
Drawable dest_win;
int x,y,dx,dy,ex,ey;
{
    Display *dpy = XtDisplay(w);
    GC gc;
    Drawable source_win = XtWindow(source_widget);
    gc = getgc(w);
    if (!source_widget) source_widget = (Widget)w;
    if (!dx) dx = source_widget->core.width - x;
    if (!dy) dy = source_widget->core.height - y;

    if (XtIsSubclass(source_widget, xpwPixmapWidgetClass)) {
        XpwPixmapWidget s = (XpwPixmapWidget)source_widget;
        if (HasPixmap(s))
            source_win = (Drawable)s->xpwpixmap.pixmap;
    }

    if (source_win && dest_win)
        XCopyArea(dpy, source_win, dest_win, gc, x,y,dx,dy,ex,ey);
}


static void CopyTo(w,dest_widget,x,y,dx,dy,ex,ey)
XpwGraphicWidget w;
Widget dest_widget;
int x,y,dx,dy,ex,ey;
{
    Display *dpy = XtDisplay(w);
    GC gc;
    Drawable dest_win, source_win;

    gc = getgc(w);

    if (HasPixmap(w))
        source_win = w->xpwpixmap.pixmap;
    else
        source_win = XtWindow(w);

    if (!dest_widget) dest_widget = (Widget)w;
    dest_win = XtWindow(dest_widget);

    if (!dx) dx = w->core.width - x;
    if (!dy) dy = w->core.height - y;
    if (source_win && dest_win)
        XCopyArea(dpy, source_win, dest_win, gc, x,y,dx,dy,ex,ey);
    if (XtIsSubclass(dest_widget, xpwPixmapWidgetClass) &&
            XtClass(dest_widget) != xpwPixmapWidgetClass) {
        XpwPixmapWidget d = (XpwPixmapWidget)dest_widget;
        if (HasPixmap(d)) {
            dest_win = (Drawable)d->xpwpixmap.pixmap;
            if (source_win && dest_win)
                XCopyArea(dpy, source_win, dest_win, gc, x,y,dx,dy,ex,ey);
        }
    }
}


static XpwMethodRet GetImage(w, x, y, width, height, mask, format)
XpwPixmapWidget w;
int x,y,width,height, format;
unsigned long mask;
{
    Drawable src;
    XpwMethodRet res = 0;
    if (!mask) mask = AllPlanes;
    if (!format) {
        if (w->core.depth > 1) format = ZPixmap;
        else format = XYPixmap;
    }
    if (HasPixmap(w))
        res=(XpwMethodRet)XGetImage(XtDisplay(w),w->xpwpixmap.pixmap,
        x,y,width,height,mask,format);
    else
        res=(XpwMethodRet)XGetImage(XtDisplay(w),XtWindow(w),
        x,y,width,height,mask,format);
    return(res);
}

static XpwMethodRet CreateImage(w,width, height, depth, image_array)
XpwPixmapWidget w;
unsigned int width, height, depth;
char *image_array;
{
    register Screen *screen = XtScreen(w);
    register Display *dpy = XtDisplay(w);
    XImage *image;
    GC gc = w->xpwcore.users_gc;
    image = XCreateImage(dpy, DefaultVisualOfScreen(screen),
                 depth, (depth == 1) ? XYBitmap : ZPixmap,
                 0, image_array,
                 width, height,8,0);
    return((XpwMethodRet)image);
}

static void PutImage(w, image, x,y, dst_x, dst_y, width, height)
XpwPixmapWidget w;
unsigned int width, height;
int x,y, dst_x, dst_y;
XImage *image;
{
    register Screen *screen = XtScreen(w);
    register Display *dpy = XtDisplay(w);
    GC gc = w->xpwcore.users_gc;
    if (HasPixmap(w)) {
        XPutImage(dpy, w->xpwpixmap.pixmap, gc, image,
                x,y,dst_x, dst_y, width, height);
        if (XtClass(w) != xpwPixmapWidgetClass)
            _XpwSyntheticExposeEvent(w, dst_x, dst_y, width, height);
    } else if (XtClass(w) != xpwPixmapWidgetClass)
        XPutImage(dpy, XtWindow(w), gc, image,
                x,y, dst_x, dst_y, width, height);

}

static void CreatePutImage(w,width, height, x,y,image_array,depth)
XpwPixmapWidget w;
unsigned int width, height;
int x,y, depth;
char *image_array;
{
    XImage *image;
    image = (XImage*)CreateImage(w, width, height, depth,image_array);
    PutImage(w, image, 0,0,x,y, width, height);
    XDestroyImage(image);
}

/* TAKEN FROM Xmu Code */

static XArc arcs[8];
static void DrawRoundedRect (dpy, draw, gc, x, y, w, h, ew, eh)
    Display     *dpy;
    Drawable        draw;
    GC          gc;
    int         x, y, w, h, ew, eh;
{

    if (ew*2 > w) ew = w / 2;
    if (eh*2 > h) eh = h / 2;

    arcs[0].x = x;
    arcs[0].y = y;
    arcs[0].width = ew*2;
    arcs[0].height = eh*2;
    arcs[0].angle1 = 180*64;
    arcs[0].angle2 = -90*64;

    arcs[1].x = x + ew;
    arcs[1].y = y;
    arcs[1].width = w - ew*2;
    arcs[1].height = 0;
    arcs[1].angle1 = 180*64;
    arcs[1].angle2 = -180*64;

    arcs[2].x = x + w - ew*2;
    arcs[2].y = y;
    arcs[2].width = ew*2;
    arcs[2].height = eh*2;
    arcs[2].angle1 = 90*64;
    arcs[2].angle2 = -90*64;

    arcs[3].x = x + w;
    arcs[3].y = y + eh;
    arcs[3].width = 0;
    arcs[3].height = h - eh*2;
    arcs[3].angle1 = 90 * 64;
    arcs[3].angle2 = -180*64;

    arcs[4].x = x + w - ew*2;
    arcs[4].y = y + h - eh*2;
    arcs[4].width = ew * 2;
    arcs[4].height = eh * 2;
    arcs[4].angle1 = 0;
    arcs[4].angle2 = -90*64;

    arcs[5].x = x + ew;
    arcs[5].y = y + h;
    arcs[5].width = w - ew*2;
    arcs[5].height = 0;
    arcs[5].angle1 = 0;
    arcs[5].angle2 = -180*64;

    arcs[6].x = x;
    arcs[6].y = y + h - eh*2;
    arcs[6].width = ew*2;
    arcs[6].height = eh*2;
    arcs[6].angle1 = 270*64;
    arcs[6].angle2 = -90*64;

    arcs[7].x = x;
    arcs[7].y = y + eh;
    arcs[7].width = 0;
    arcs[7].height = h - eh*2;
    arcs[7].angle1 = 270*64;
    arcs[7].angle2 = -180*64;
    XDrawArcs (dpy, draw, gc, arcs, 8);
}

static void FillRoundedRect (dpy, draw, gc, x, y, w, h, ew, eh)
    Display     *dpy;
    Drawable        draw;
    GC          gc;
    int         x, y, w, h, ew, eh;
{
    XRectangle rects[3];
    XGCValues vals;

    XGetGCValues(dpy, gc, GCArcMode, &vals);
    if (vals.arc_mode != ArcPieSlice) XSetArcMode(dpy, gc, ArcPieSlice);

    if (ew*2 > w) ew = w / 2;
    if (eh*2 > h) eh = h / 2;

    arcs[0].x = x;
    arcs[0].y = y;
    arcs[0].width = ew*2;
    arcs[0].height = eh*2;
    arcs[0].angle1 = 180*64;
    arcs[0].angle2 = -90*64;

    arcs[1].x = x + w - ew*2;
    arcs[1].y = y;
    arcs[1].width = ew*2;
    arcs[1].height = eh*2;
    arcs[1].angle1 = 90*64;
    arcs[1].angle2 = -90*64;

    arcs[2].x = x + w - ew*2;
    arcs[2].y = y + h - eh*2;
    arcs[2].width = ew*2;
    arcs[2].height = eh*2;
    arcs[2].angle1 = 0;
    arcs[2].angle2 = -90*64;

    arcs[3].x = x;
    arcs[3].y = y + h - eh*2;
    arcs[3].width = ew*2;
    arcs[3].height = eh*2;
    arcs[3].angle1 = 270*64;
    arcs[3].angle2 = -90*64;

    XFillArcs (dpy, draw, gc, arcs, 4);

    rects[0].x = x + ew;
    rects[0].y = y;
    rects[0].width = w - ew*2;
    rects[0].height = h;

    rects[1].x = x;
    rects[1].y = y + eh;
    rects[1].width = ew;
    rects[1].height = h - eh*2;

    rects[2].x = x + w - ew;
    rects[2].y = y + eh;
    rects[2].width = ew;
    rects[2].height = h - eh*2;

    XFillRectangles (dpy, draw, gc, rects, 3);

    if (vals.arc_mode != ArcPieSlice) XSetArcMode(dpy, gc, vals.arc_mode);
}

#define DEF_DRAWSTRING(Name, XDrawP, XmbDrawP)                              \
static void Name(dpy, d, font_set, gc, x, y, string, num_bytes)             \
  Display *dpy;                                                             \
  Drawable d;                                                               \
  XFontSet font_set;                                                        \
  GC gc;                                                                    \
  int x, y;                                                                 \
  char *string;                                                             \
  int num_bytes;                                                            \
  { XFontStruct **font_struct_list;                                         \
    char **font_name_list;                                                  \
    if (XFontsOfFontSet(font_set, &font_struct_list, &font_name_list) == 1) \
        /* If there's only one font, assume straightforward 8-bit working   \
         * and use XDrawString etc (the font is already in the gc).         \
         * Gets around a bug in Nutcracker XmbDrawString which apparently   \
         * truncates chars to 7 bits in the standard locale.                \
         */                                                                 \
        XDrawP(dpy, d, gc, x, y, string, num_bytes);                        \
    else                                                                    \
        XmbDrawP(dpy, d, font_set, gc, x, y, string, num_bytes);            \
  }

DEF_DRAWSTRING(DrawString,      XDrawString,      XmbDrawString)
DEF_DRAWSTRING(DrawImageString, XDrawImageString, XmbDrawImageString)

externaldef (_xpwpixmapmethods)
XpwMethod _xpwPixmapMethods[] = {
/*  {id,                    proc,           nargs,  flags} */
#define M METHOD_STRUCT
M   (XpwMDrawPoint,         XDrawPoint,     2,  RequiresGC | RequiresDrawable),
M   (XpwMDrawPoints,        XDrawPoints,    3,  RequiresGC | RequiresDrawable),
M   (XpwMDrawLine,          XDrawLine,      4,  RequiresGC | RequiresDrawable),
M   (XpwMDrawLines,         XDrawLines,     3,  RequiresGC | RequiresDrawable),
M   (XpwMDrawSegments,      XDrawSegments,  2,  RequiresGC | RequiresDrawable),
M   (XpwMDrawArc,           XDrawArc,       6,  RequiresGC | RequiresDrawable),
M   (XpwMDrawArcs,          XDrawArcs,      2,  RequiresGC | RequiresDrawable),
M   (XpwMDrawRectangle,     XDrawRectangle, 4,  RequiresGC | RequiresDrawable),
M   (XpwMDrawRectangles,    XDrawRectangles,2,  RequiresGC | RequiresDrawable),
M   (XpwMDrawRoundedRectangle, DrawRoundedRect, 6,  RequiresGC | RequiresDrawable),
M   (XpwMDrawString,        DrawString,     4,  RequiresGC | RequiresFontSet | RequiresDrawable),
M   (XpwMDrawImageString,   DrawImageString,4,  RequiresGC | RequiresFontSet | RequiresDrawable),
M   (XpwMFillArc,           XFillArc,       6,  RequiresGC | RequiresDrawable),
M   (XpwMFillArcs,          XFillArcs,      2,  RequiresGC | RequiresDrawable),
M   (XpwMFillRectangle,     XFillRectangle, 4,  RequiresGC | RequiresDrawable),
M   (XpwMFillRectangles,    XFillRectangles,2,  RequiresGC | RequiresDrawable),
M   (XpwMFillRoundedRectangle, FillRoundedRect, 6,  RequiresGC | RequiresDrawable),
M   (XpwMFillPolygon,       XFillPolygon,   4,  RequiresGC | RequiresDrawable),
M   (XpwMPutImage,          PutImage,       7,  RequiresWidget | Cut),
M   (XpwMGetImage,          GetImage,       6,  RequiresWidget | Cut),
M   (XpwMCreateImage,       CreateImage,    4,  RequiresWidget | Cut),
M   (XpwMCreatePutImage,    CreatePutImage, 6,  RequiresWidget | Cut),
M   (XpwMClearArea,         ClearPixmapArea,4,  RequiresWidget),
M   (XpwMClearWindow,       ClearPixmap,    0,  RequiresWidget),
M   (XpwMCopyFrom,          CopyFrom,       7,  RequiresWidget | RequiresDrawable),
M   (XpwMCopyTo,            CopyTo,         7,  RequiresWidget | Cut),
#undef M
};


externaldef (_num_xpwpixmapmethods)
int _num_xpwPixmapMethods = XtNumber(_xpwPixmapMethods);


/****************************************************************
 *
 * XpwGraphic - Method Table Declaration
 *
 ****************************************************************/

extern void         /* defined here, used internally (XpwGraphic.c) */
            _XpwFreeColors(),
            _XpwFreeColormap();

static XpwMethodRet /* methods with return values */
            AllocColorRange(),
            AllocStoreColor();

static void /* basic methods */
            SetPixelColor(),
            ClearGraphic(),
            ClearGraphicArea(),
            CreateColormap();

/****************************************************************
 *
 *  Method definitions
 *
 ****************************************************************/

static void ClearGraphic(w)
XpwPixmapWidget w;
{
    if (XtWindow(w))
        XClearArea(XtDisplay(w), XtWindow(w), 0,0,0,0, TRUE);
    /* need to generate exposures so that the (possibly tiled)
    pixmap is repainted */
}

static void ClearGraphicArea(w,x,y,width,height)
XpwGraphicWidget w;
int x,y,width,height;
{
    register Display *dpy = XtDisplay(w);
    register Window win = XtWindow(w);

    if (!width) width=w->core.width;
    if (!height) height=w->core.height;

    XClearArea(dpy, win,  x,y, width, height, TRUE);
    /* exposures needed so pixmaps background tiling is copied to
      window */
}

/*************************************************************************
 *                  Colormaps
 ************************************************************************/

#define XtColormap(widget) ((widget)->core.colormap)

static void CreateColormap(w)
XpwGraphicWidget w;
{
    register Screen *screen = XtScreen(w);
    register Display *dpy = XtDisplay(w);
    Window win = XtWindow(w);
    /* free any existing colormap */
    if (XtColormap(w) && XtColormap(w) != DefaultColormapOfScreen(screen))
        _XpwFreeColormap(w);

    /* get new colormap */
    XtColormap(w) = XCreateColormap(dpy, win,
                    DefaultVisualOfScreen(screen),
                    AllocNone);

    /* install it for window */
    XSetWindowColormap(dpy, win, XtColormap(w));

    if (XtIsShell(w->core.parent))
        XSetWindowColormap(dpy, XtWindow(w->core.parent),
            XtColormap(w));
    else {
        Widget parent=w->core.parent;
        Atom colormapProp;
        /* find top shell */
        while (!XtIsShell(parent)) parent=(Widget)parent->core.parent;
        /* add colormap to list of colormaps for shell */
        colormapProp = XInternAtom(dpy,"WM_COLORMAP_WINDOWS",False);
        XChangeProperty(dpy, XtWindow(parent), XA_WINDOW,
            colormapProp, 32, PropModeAppend, (unsigned char*)&win,1);
    }

}

void _XpwFreeColormap(w)
XpwGraphicWidget w;
{
    register Display *dpy = XtDisplay(w);
    Screen *screen = XtScreen(w);
    Window win = XtWindow(w);
    Colormap cmap = XtColormap(w);
    XpwColorList *clist=w->xpwgraphic.allocated_colors->next;

    /* free any colors in the allocated_colors that were attached to the
       colormap that is being freed.
    */
    while (clist) {
        XpwColorList *next =clist->next;
        if (clist->cmap == cmap && clist->colors)
            _XpwFreeColors(w, clist);
        clist = next;
    }

    if (cmap == 0 || cmap == DefaultColormapOfScreen(screen)) return;
    /* free colormap */
    XFreeColormap(dpy, XtColormap(w));
    /* set colormap and window attributes to default colormap */
    XtColormap(w) = DefaultColormapOfScreen(screen);
    XSetWindowColormap(dpy, win, XtColormap(w));
    if (XtIsShell(w->core.parent))
        XSetWindowColormap(dpy, XtWindow(w->core.parent),
            XtColormap(w));
    else {
        Widget parent=w->core.parent;
        Atom colormapProp;
        /* find top shell */
        while (!XtIsShell(parent)) parent=(Widget)parent->core.parent;
        /* add colormap to list of colormaps for shell */
        colormapProp = XInternAtom(dpy,"WM_COLORMAP_WINDOWS",False);
        XChangeProperty(dpy, XtWindow(parent), XA_WINDOW,
            colormapProp, 32, PropModeReplace, (unsigned char*)&win,1);
    }
}

/*************************************************************************
 *                  Colors
 ************************************************************************/

/* Any colors allocated to a XpwGraphic widget are remembered in the
   allocated_colors list - so that they can be freed on destroy time.
*/


static XpwColorList *AddColors(w, colors, num_colors, exact, read_only)
XpwGraphicWidget w;
XColor *colors;
Cardinal num_colors;
Boolean exact, read_only;
{
    XpwColorList  *allocated_colors = w->xpwgraphic.allocated_colors,
                    *next;

    while (allocated_colors->next)
        allocated_colors =allocated_colors->next;

    next = (XpwColorList *)XtNew(XpwColorList);
    next->colors = colors;
    next->num_colors = num_colors;
    next->cmap = XtColormap(w);
    next->exact = exact;
    next->read_only = read_only;
    next->next = NULL;
    allocated_colors->next = next;
    return(next);
}

/* Free a range of colors */
static void FreeColors(w, colorlist)
XpwGraphicWidget w;
XpwColorList *colorlist;
{
    _XpwFreeColors(w,colorlist);
}

void _XpwFreeColors(w, colorlist)
XpwGraphicWidget w;
XpwColorList *colorlist;
{
    register Display *dpy = XtDisplay(w);
    Colormap cmap;
    XpwColorList *prev, *allocated_colors = w->xpwgraphic.allocated_colors;
    XColor *colors;
    int i;
    while (allocated_colors->next && allocated_colors != colorlist) {
        prev = allocated_colors;
        allocated_colors =allocated_colors->next;
    }
    if (allocated_colors == colorlist && colorlist->colors) {
        colors = colorlist->colors;
        cmap = colorlist->cmap;
        for(i=0;i<colorlist->num_colors;i++)
            if (colors[i].pixel != 0)
                XFreeColors(dpy, cmap, &colors[i].pixel,1,0);
        prev->next = allocated_colors->next;
        XtFree((char *)colorlist);
    } else
        _XpwMethodWarning(w, XpwMFreeColor, "invalidColors", "",
            "The widget does not own the specified colour(s)");
}

/* Allocate num_cells of colours in the colourmap, determining their
   Colours as a linear range whose start rgb is r1,g1,g1 and end rgb is
   r2,g2,b2
*/
#define mfac 256
static XpwMethodRet AllocColorRange(w,num_cells, r1,g1,b1,r2,g2,b2)
XpwGraphicWidget w;
int num_cells, r1, g1, b1, r2, g2, b2;
{
    register Display *dpy = XtDisplay(w);
    Colormap cmap = XtColormap(w);
    XColor *colors;
    Pixel *pixels, pixel;
    int i, dr, dg, db, found_colors = False;
    unsigned long plane_masks[1];

    pixels = (Pixel *)XtCalloc(num_cells, (Cardinal)sizeof(Pixel));

    /* we must set the rgb values in a colors array: */
    /* try to allocate read/write color cells */
    if (XAllocColorCells(dpy, cmap, TRUE, plane_masks, 0,
                    pixels,num_cells)) {
        colors = (XColor *)XtCalloc(num_cells, (Cardinal)sizeof(XColor));
        /* dr,dg and db are distance to go from start to end rgb vals*/
        dr = mfac * (r2-r1) / (num_cells-1);
        dg = mfac * (g2-g1) / (num_cells-1);
        db = mfac * (b2-b1) / (num_cells-1);
        r1 *= mfac; g1 *= mfac; b1 *= mfac;
        for(i=0; i< num_cells; i++) {
            colors[i].red = (Pixel)r1;
            colors[i].green = (Pixel)g1;
            colors[i].blue = (Pixel)b1;
            colors[i].flags = DoRed | DoBlue | DoGreen;
            colors[i].pixel = pixels[i];
            r1+=dr; g1+=dg; b1+=db;
        }
        XStoreColors(dpy, cmap, colors, num_cells);
        found_colors = True;
    }
    XtFree((char *)pixels);

    if (!found_colors) return((XpwMethodRet)NULL);
    else return((XpwMethodRet)AddColors(w, colors, num_cells,TRUE,TRUE));
}


static XpwMethodRet AllocStoreColor(w, r, g, b)
XpwGraphicWidget w;
int r,g,b;
{
    register Screen *screen = XtScreen(w);
    register Display *dpy = XtDisplay(w);
    register Window win = XtWindow(w);
    Colormap cmap = XtColormap(w);
    XColor aColor;
    unsigned long plane_masks[1], carray[1];

    if (!XAllocColorCells(dpy, cmap, 1, plane_masks, 0, carray,1))
        return (XpwMethodRet)-1;

    aColor.red = r << 8;
    aColor.green= g << 8;
    aColor.blue = b << 8;
    aColor.pixel = carray[0];
    aColor.flags = DoRed | DoBlue | DoGreen;

    XStoreColor(dpy, cmap, &aColor);
    return((XpwMethodRet)carray[0]);
}

static void SetPixelColor(w, i, r, g, b)
XpwGraphicWidget w;
int i,r,g,b;
{
/* this one need some error checking. */
    register Screen *screen = XtScreen(w);
    register Display *dpy = XtDisplay(w);
    register Window win = XtWindow(w);
    Colormap cmap = XtColormap(w);
    XColor aColor;


    aColor.red = r << 8;
    aColor.green= g << 8;
    aColor.blue = b << 8;
    aColor.pixel = i;
    aColor.flags = DoRed | DoBlue | DoGreen;
    XStoreColor(dpy, cmap, &aColor);
}


externaldef (xpwgraphicmethods)
XpwMethod _xpwGraphicMethods[] = {
/*  {id,                    proc,               nargs, flags}*/
#define M METHOD_STRUCT
M   (XpwMClearWindow,       ClearGraphic,       0,  RequiresWidget),
M   (XpwMClearArea,         ClearGraphicArea,   4,  RequiresWidget),
M   (XpwMCreateColormap,    CreateColormap,     0,  RequiresWidget),
M   (XpwMFreeColormap,      _XpwFreeColormap,   0,  RequiresWidget),
M   (XpwMAllocColorRange,   AllocColorRange,    7,  RequiresWidget),
M   (XpwMAllocStoreColor,   AllocStoreColor,    3,  RequiresWidget),
M   (XpwMFreeColorRange,    _XpwFreeColors,     1,  RequiresWidget),
M   (XpwMSetPixelColor,     SetPixelColor,      4,  RequiresWidget),
#undef M
};

externaldef (_num_xpwgraphicmethods)
int _num_xpwGraphicMethods = XtNumber(_xpwGraphicMethods);



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 21 1997
        Made XpwMDraw(Image)String methods call procedures which revert
        to using XDraw(Image)String if the fontset consists of a single
        font.
--- John Gibson, Apr  8 1997
        Changed XpwMDraw(Image)String methods to use XmbDraw(Image)String
        with XtNfontSet.
--- John Gibson, Dec 19 1993
        Fixed ANSI warnings
--- Adrian Howard, Jun  1 1993
        Installed improvements from Jon Meyer:
            i) Altered explicit tests of pixmap_status to use HasPixmap
            (see XpwPixmap.h)
            ii) CopyTo/From & ClearGraphic check for windows existance
--- Adrian Howard, Jul  1 1992
        Added fixed from JonM for -XpwAllocColorRange- & -XpwFreeColorRange-.
--- Adrian Howard, Jun 30 1992
        Fixed -DrawRoundedRect- and -FillRoundedRect- (pointed out by JonM)
--- John Gibson, Mar  9 1992
        VMS externaldef mods
--- Jonathan Meyer, Jan 12 1992
    Changed to use _XpwSyntheticExposeEvent, which prevents flickering
    when an image is copied onto the widget.
--- John Gibson, Dec 17 1991
        Replaced erroneous ' after declaration on line 408 with ;
--- Jonathan Meyer, Dec 17 1991
        Removed V*map code
--- John Gibson, Oct 31 1991
        VMS mods
--- Jonathan Meyer, Sep 25 1991
    Fixed flags in method table for XpwPutImage.
--- Jonathan Meyer, Jul 30 1991
    Made AllocStoreColor return -1 on failure
--- Jonathan Meyer, Jul  9 1991
    Fixed bug in CreatePutImage
--- Jonathan Meyer, Jan 29 1991
    Changed require_colors to colors for AllocColorRange/XStoreColors
--- Jonathan Meyer, Jan 27 1991 Changed XA_COLORMAP to XA_WINDOW
--- Jonathan Meyer, Oct 19 1990
    Removed XDraw and XDrawFilled - out of date
--- Jonathan Meyer, Aug 21 1990
    Restored "X11/" prefix, renamed file Graphics.c.
    Defined XpwNotInstalled. Removed some redundant #include's.
--- Jonathan Meyer, Aug 15 1990
    Changed ClearWindow and ClearArea so that they respect background
    pixmaps (ie. tiling).
--- Jonathan Meyer, Aug  8 1990
    Changed CreatePutImage to use XClearArea rather than XClearWindow
--- Andreas Schoter, Jul 23 1990 Changed all pop* identifiers to xpw*
--- Andreas Schoter, 16 July 1990
    Replaced all occurances of Pop* variable names with Xpw*
--- Simon Nichols, Jun 19 1990
        Removed "X11/" prefix from #include file names.
--- James Goodlet, May 24 1990 - switch for X11R4.
--- Jonathan Meyer, 15 Dec 1989 - Major upgrade to Xpw revision 3.
 */
