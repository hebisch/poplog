/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XFonts.p
 > Purpose:
 > Author:          Gareth Palmer,  3 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;
uses XExtent;


global constant macro (

    /* used in QueryFont -- draw direction */

    FontLeftToRight     = 0,
    FontRightToLeft     = 1,

    FontChange          = 255,

);


external declare XFonts in c;
    (external_import_procedure XptImportProcedure)


    Font XLoadFont(display, name)
    Display *display;
    char *name;
    {}

    XFontStruct *XLoadQueryFont(display, name)
    Display *display;
    char *name;
    {}

    void XUnloadFont(display, font)
    Display *display;
    Font font;
    {}

    void XFreeFont(display, font_struct)
    Display *display;
    XFontStruct *font_struct;
    {}

    void XFreeFontInfo(names, info, actual_count)
    char **names;
    XFontStruct *info;
    int actual_count;
    {}

    void XFreeFontNames(list)
    char *list;
    {}

    void XFreeFontPath(list)
    char **list;
    {}

    char **XListFonts(display, pattern, maxnames, actual_count)
    Display *display;
    char *pattern;
    int maxnames;
    int *actual_count;          ;;; RETURN
    {}

    char **XListFontsWithInfo(display, pattern, maxnames, count, info)
    Display *display;
    char *pattern;              ;;; null-terminated
    int maxnames;
    int *count;                 ;;; RETURN
    XFontStruct **info;         ;;; RETURN
    {}

    XFontStruct *XQueryFont(display, font_ID)
    Display *display;
    XID font_ID;
    {}

    void XSetFont(display, gc, font)
    Display *display;
    GC gc;
    Font font;
    {}

    void XSetFontPath(display, directories, ndirs)
    Display *display;
    char **directories;
    int ndirs;
    {}

    char **XGetFontPath(display, npaths)
    Display *display;
    int *npaths;                ;;; RETURN number of elements
    {}

    ;;; duplicated from Properties
    Bool XGetFontProperty(font_struct, atom, value)
    XFontStruct *font_struct;
    Atom atom;
    unsigned long *value;           ;;; RETURN
    {}

    ;;; duplicated from Cursors
    Cursor XCreateFontCursor(display, shape)
    Display *display;
    unsigned int shape;
    {}

endexternal;


xlib_external_require XFonts;


global vars XFonts = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
