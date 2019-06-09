/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XText.p
 > Purpose:
 > Author:          Gareth Palmer,  3 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;
uses XExtent;

external declare XText in c;
    (external_import_procedure XptImportProcedure)


    /*
     * PolyText routines take these as arguments.
     */
    typedef struct {
        char *chars;        ;;; pointer to string */
        int nchars;         ;;; number of characters */
        int delta;          ;;; delta between strings */
        Font font;          ;;; font to print it in, None don't change */
    } XTextItem;

    typedef struct {        ;;; normal 16 bit characters are two bytes */
        unsigned char byte1;
        unsigned char byte2;
    } XChar2b;

    typedef struct {
        XChar2b *chars;     ;;; two byte characters */
        int nchars;         ;;; number of characters */
        int delta;          ;;; delta between strings */
        Font font;          ;;; font to print it in, None don't change */
    } XTextItem16;


    void XDrawImageString(display, drawable, gc, x, y, string, length)
    Display *display;
    Drawable drawable;
    GC gc;
    int x, y;
    char *string;
    int length;
    {}

    void XDrawImageString16(display, drawable, gc, x, y, string, length)
    Display *display;
    Drawable drawable;
    GC gc;
    int x, y;
    XChar2b *string;
    int length;
    {}

    void XDrawString(display, drawable, gc, x, y, string, length)
    Display *display;
    Drawable drawable;
    GC gc;
    int x, y;
    char *string;
    int length;
    {}

    void XDrawString16(display, drawable, gc, x, y, string, length)
    Display *display;
    Drawable drawable;
    GC gc;
    int x, y;
    XChar2b *string;
    int length;
    {}

    void XDrawText(display, drawable, gc, x, y, items, nitems)
    Display *display;
    Drawable drawable;
    GC gc;
    int x, y;
    XTextItem *items;
    int nitems;
    {}

    void XDrawText16(display, drawable, gc, x, y, items, nitems)
    Display *display;
    Drawable drawable;
    GC gc;
    int x, y;
    XTextItem16 *items;
    int nitems;
    {}

    void XQueryTextExtents(display, font_ID, string, nchars, direction, ascent,
                            descent, overall)
    Display *display;
    XID font_ID;
    char *string;
    int nchars;
    int *direction;               ;;; RETURN
    int *ascent, *descent;        ;;; RETURN
    XCharStruct *overall;         ;;; RETURN
    {}

    void XQueryTextExtents16(display, font_ID, string, nchars, direction, ascent,
                            descent, overall)
    Display *display;
    XID font_ID;
    XChar2b *string;
    int nchars;
    int *direction;               ;;; RETURN
    int *ascent, *descent;        ;;; RETURN
    XCharStruct *overall;         ;;; RETURN
    {}

    void XTextExtents(font_struct, string, nchars, direction, ascent, descent,
                        overall)
    XFontStruct *font_struct;
    char *string;
    int nchars;
    int *direction;             ;;; RETURN
    int *ascent, *descent;      ;;; RETURN
    XCharStruct *overall;       ;;; RETURN
    {}

    void XTextExtents16(font_struct, string, nchars, direction, ascent, descent,
                        overall)
    XFontStruct *font_struct;
    XChar2b *string;
    int nchars;
    int *direction;             ;;; RETURN
    int *ascent, *descent;      ;;; RETURN
    XCharStruct *overall;       ;;; RETURN
    {}

    int XTextWidth(font_struct, string, count)
    XFontStruct *font_struct;
    char *string;
    int count;
    {}

    int XTextWidth16(font_struct, string, count)
    XFontStruct *font_struct;
    XChar2b *string;
    int count;
    {}

endexternal;


xlib_external_require XText;


global vars XText = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
