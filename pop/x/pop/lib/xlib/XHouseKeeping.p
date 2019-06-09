/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XHouseKeeping.p
 > Purpose:         General Xlib routines
 > Author:          Ian Rogers, Apr 13 1989 (see revisions)
 > Documentation:   REF * XHouseKeeping
 > Related Files:
 */

compile_mode: pop11 +strict;
section;

uses XConstants;

external declare XHouseKeeping in c;
    (external_import_procedure XptImportProcedure)

    Display *XOpenDisplay(name)
        char *name;
    {}

    void XCloseDisplay(display)
        Display *display;
    {}

    void XFree(data)
        caddr_t data;
    {}

    void XNoOp(display)
        Display *display;
    {}

    void XFlush(display)
        Display *display;
    {}

    void XSync(display, discard)
        Display *display;
        int discard;
    {}

endexternal;

xlib_external_require XHouseKeeping;

global vars XHouseKeeping = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun 30 1993
     #  DefaultScreen now autoloadable to avoid code duplication
     #  Tidied, sectioned, made +strict
--- Adrian Howard, Jan 23 1992 : Uncommented -XCloseDisplay- as it now works
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
