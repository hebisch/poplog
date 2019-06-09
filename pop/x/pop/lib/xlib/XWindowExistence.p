/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XWindowExistence.p
 > Purpose:
 > Author:          Gareth Palmer,  3 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;
uses XWindowConstants;

global constant macro (

    /* Window classes used by CreateWindow */
    /* Note that CopyFromParent is already defined as 0 above */

    InputOutput     = 1,
    InputOnly       = 2,

);

external declare XWindowExistence in c;
    (external_import_procedure XptImportProcedure)


    Window XCreateSimpleWindow(display, parent, x, y, width, height, border_width,
                                border, background)
    Display *display;
    Window parent;
    int x, y;
    unsigned int width, height, border_width;
    unsigned long border;
    unsigned long background;
    {}

    Window XCreateWindow(display, parent, x, y, width, height, border_width,
                            depth, class, visual, valuemask, attributes)
    Display *display;
    Window parent;
    int x, y;
    unsigned int width, height;
    unsigned int border_width;
    int depth;
    unsigned int class;
    Visual *visual;
    unsigned long valuemask;
    XSetWindowAttributes *attributes;
    {}

    void XDestroySubwindows(display, w)
    Display *display;
    Window w;
    {}

    void XDestroyWindow(display, w)
    Display *display;
    Window w;
    {}


endexternal;


xlib_external_require XWindowExistence;


global vars XWindowExistence = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
