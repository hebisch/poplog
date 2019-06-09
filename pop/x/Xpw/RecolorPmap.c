/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 * File:            C.x/x/Xpw/RecolorPmap.c
 * Version:         Revision 5.0
 * Purpose:     Change foreground and background colours of pixmap
 * Author:          Jonathan Meyer, Aug 23 1990
 * Documentation:
 * Related Files:   XpwCore.c XpwPixmap.c
 */

#include <X11/Xlib.h>

/* XpwRecolorPixmap() - change foreground and background colours of pixmap

   pixmap = XpwRecolorPixmap(screen, src_pixmap,
            src_foreground, src_background,
            dst_foreground, dst_background,
            dst_depth,free);
   Screen *screen;
   Pixmap *src_pixmap;
   unsigned long src_foreground, src_background;
   unsigned long dst_foreground, dst_background;
   unsigned long dst_depth;
   Boolean free;

   Performs two functions:

   1) Convert a one-bit deep bitmap into a pixmap with a depth of dst_depth
    and the specified dst_foreground and dst_background pixel values.
    The src_foreground and src_background pixel values should be either
    0 or 1 and specify which of the two colours in the bitmap represent
    the foreground and background colours respectively.
or

   2) Convert a pixmap containing only two colours (src_foreground and
    src_background) into a pixmap (of possibly different depth)
    using the new colours dst_foreground and dst_background.

   If -free- is TRUE, then an XFreePixmap is used to release the src_pixmap.

   Used by Initialize procs of XpwCore and XpwPixmap to convert bitmaps
   To pixmaps and recolour pixmaps.

*/

static unsigned long choose_plane(pixel_value)
unsigned long pixel_value;
{
    /* choose first bit that is set in pixel_value */
    unsigned long plane_mask = 1;
    while (! (plane_mask & (unsigned long)pixel_value)) plane_mask <<=1;
    return(plane_mask);
}

Pixmap XpwRecolorPixmap(screen,src_pix,
            src_fg,src_bg,
            dst_fg,dst_bg, dst_depth, free)
Screen *screen;
Pixmap src_pix;
unsigned long src_fg, src_bg, dst_fg,dst_bg;
unsigned long dst_depth;
int free;
{
    register Display *dpy = DisplayOfScreen(screen);
    Pixmap dst_pix;
    Drawable drawable = (Drawable)src_pix;
    unsigned int width, height, src_depth, dummy;
    XGCValues values;
    unsigned long valuemask = GCForeground | GCBackground;
    GC gc;
    if (!XGetGeometry(dpy, src_pix, (Window*)&dummy,
            (int*)&dummy, (int*)&dummy,
            &width, &height,
            &dummy, &src_depth)) return(0);


    dst_pix = XCreatePixmap(dpy, drawable, width, height, dst_depth);
    if (src_depth <= 1) {
        /* converting bitmap to pixmap:
         * set the stipple of a GC, and perform a FillRectangle
         */
        values.foreground = dst_fg;
        values.background = dst_bg;
        values.stipple = src_pix;
        values.fill_style = FillOpaqueStippled;
        valuemask |= GCStipple | GCFillStyle;
        gc = XCreateGC(dpy, dst_pix, valuemask, &values);
        XFillRectangle(dpy, dst_pix, gc, 0,0, width,height);
        XFreeGC(dpy, gc);
    } else {
        /* recolouring a two-colour pixmap with new fg/bg: */
        unsigned long plane;
        if (src_fg == 0) {
            values.background = dst_fg;
            values.foreground = dst_bg;
            plane = choose_plane(src_bg);
        } else if (src_bg == 0) {
            values.background = dst_bg;
            values.foreground = dst_fg;
            plane = choose_plane(src_fg);
        } else {
            /* tricky - set all src_bg pixels in src_pix to 0 by
             * performing an XOR'ed fill on src_pix with the
             * colour src_bg - all src_bg pixels become 0; all
             * src_fg pixels become (src_fg XOR src_bg) (non 0).
             */
            values.foreground = src_bg;
            valuemask |= GCFunction ;
            values.function = GXxor;
            gc = XCreateGC(dpy, src_pix, valuemask, &values);
            XFillRectangle(dpy, src_pix, gc, 0,0, width,height);
            XFreeGC(dpy, gc);
            valuemask ^= GCFunction;
            values.background = dst_bg;
            values.foreground = dst_fg;
            plane = choose_plane(src_fg ^ src_bg);
        }
        gc = XCreateGC(dpy, dst_pix, valuemask, &values);
        XCopyPlane(dpy, src_pix, dst_pix, gc, 0,0,
                     width, height, 0,0,plane);
        XFreeGC(dpy, gc);
    }
    if (free) XFreePixmap(dpy, src_pix);
    return(dst_pix);
}
