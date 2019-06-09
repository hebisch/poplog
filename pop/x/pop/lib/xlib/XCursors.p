/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XCursors.p
 > Purpose:
 > Author:          Gareth Palmer,  3 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */

uses XConstants;
uses XColor;
include xdefs.ph;

external declare XCursors in c;
    (external_import_procedure XptImportProcedure)

    void XDefineCursor(display, w, cursor)
    Display *display;
    Window w;
    Cursor cursor;
    {}

    void XUndefineCursor(display, w)
    Display *display;
    Window w;
    {}

    Cursor XCreateFontCursor(display, shape)
    Display *display;
    unsigned int shape;
    {}

    Cursor XCreateGlyphCursor(display, source_font, mask_font, source_char,
                                mask_char, foreground_color, background_color)
    Display *display;
    Font source_font, mask_font;
    unsigned int source_char, mask_char;
    XColor *foreground_color;
    XColor *background_color;
    {}

    Cursor XCreatePixmapCursor(display, source, mask, foreground_color,
                                background_color, x_hot, y_hot)
    Display *display;
    Pixmap source;
    Pixmap mask;
    XColor *foreground_color;
    XColor *background_color;
    unsigned int x_hot, y_hot;
    {}

    void XFreeCursor(display, cursor)
    Display *display;
    Cursor cursor;
    {}

    void XRecolorCursor(display, cursor, foreground_color, background_color)
    Display *display;
    Cursor cursor;
    XColor *foreground_color, *background_color;
    {}

    Status XQueryBestCursor(display, drawable, width, height, rwidth, rheight)
    Display *display;
    Drawable drawable;
    unsigned int width, height;
    unsigned int *rwidth, *rheight;     ;;; RETURN
    {}

    ;;; duplicated from Tile
    Status XQueryBestSize(display, class, drawable, width, height, rwidth, rheight)
    Display *display;
    int class;
    Drawable drawable;
    unsigned int width, height;
    unsigned int *rwidth, *rheight;      ;;; RETURN
    {}

endexternal;


external require XCursors;
    #_< explode(XLINK_EXLIBS) :: proglist -> proglist; >_#
endexternal;


global vars XCursors = true;

/* --- Revision History ---------------------------------------------------
--- John Gibson, May 10 1993
        Replaced XTB*ASELIBS with XLINK_EXLIBS
--- Jonathan Meyer, Jan 25 1991 Added XDEFS and XTB*ASELIBS
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
