/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XWindowMapping.p
 > Purpose:
 > Author:          Gareth Palmer,  3 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;

external declare XWindowMapping in c;
    (external_import_procedure XptImportProcedure)


void XMapRaised(display, w)
Display *display;
Window w;
{}

void XMapSubwindows(display, w)
Display *display;
Window w;
{}

void XMapWindow(display, w)
Display *display;
Window w;
{}

void XUnmapSubwindows(display, w)
Display *display;
Window w;
{}

void XUnmapWindow(display, w)
Display *display;
Window w;
{}


endexternal;


xlib_external_require XWindowMapping;


global vars XWindowMapping= true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
