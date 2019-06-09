/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XTile.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;
uses XColor;

global constant macro(

    /*
     * return codes for XReadBitmapFile and XWriteBitmapFile
     */

    BitmapSuccess       = 0,
    BitmapOpenFailed    = 1,
    BitmapFileInvalid   = 2,
    BitmapNoMemory      = 3,

);


external declare XTile in c;
    (external_import_procedure XptImportProcedure)

    Pixmap XCreatePixmap(display, drawable, width, height, depth)
    Display *display;
    Drawable drawable;
    unsigned int width, height;
    unsigned int depth;
    {}

    void XFreePixmap(display, pixmap)
    Display *display;
    Pixmap pixmap;
    {}

    Status XQueryBestSize(display, class, drawable, width, height, rwidth, rheight)
    Display *display;
    int class;
    Drawable drawable;
    unsigned int width, height;
    unsigned int *rwidth, *rheight;           ;;; RETURN
    {}

    Status XQueryBestStipple(display, drawable, width, height, rwidth, rheight)
    Display *display;
    Drawable drawable;
    unsigned int width, height;
    unsigned int *rwidth, *rheight;           ;;; RETURN
    {}

    Status XQueryBestTile(display, drawable, width, height, rwidth, rheight)
    Display *display;
    Drawable drawable;
    unsigned int width, height;
    unsigned int *rwidth, *rheight;           ;;; RETURN
    {}

    void XSetTile(display, gc, tile)
    Display *display;
    GC gc;
    Pixmap tile;
    {}

    void XSetWindowBorderPixmap(display, w, border_tile)
    Display *display;
    Window w;
    Pixmap border_tile;
    {}

    void XSetWindowBackgroundPixmap(display, w, background_tile)
    Display *display;
    Window w;
    Pixmap background_tile;
    {}

    int XReadBitmapFile(display, drawable, filename, width, height, bitmap,
                           x_hot, y_hot)
    Display *display;
    Drawable drawable;
    char *filename;
    unsigned int *width, *height;           ;;; RETURN
    Pixmap *bitmap;                         ;;; RETURN
    int *x_hot, *y_hot;                     ;;; RETURN
    {}

    int XWriteBitmapFile(display, filename, bitmap, width, height, x_hot, y_hot)
    Display *display;
    char *filename;
    Pixmap bitmap;
    unsigned int width, height;
    int x_hot, y_hot;
    {}

    Pixmap XCreateBitmapFromData(display, drawable, data, width, height)
    Display *display;
    Drawable drawable;
    char *data;
    unsigned int width, height;
    {}

    Pixmap XCreatePixmapFromBitmapData(display, drawable, data, width, heigt, fg,
                                        bg, depth)
    Display *display;
    Drawable drawable;
    char *data;
    unsigned int width, heigt;
    unsigned long fg, bg;
    unsigned int depth;
    {}

endexternal;


xlib_external_require XTile;


global vars XTile = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
