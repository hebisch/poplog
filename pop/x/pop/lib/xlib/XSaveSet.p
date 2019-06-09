/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XSaveSet.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;

global constant macro (

/* Used in ChangeSaveSet */

 SetModeInsert           = 0,
 SetModeDelete           = 1,

);


external declare XSaveSet in c;
    (external_import_procedure XptImportProcedure)


void XAddToSaveSet(display, w)
Display *display;
Window w;
{}

void XRemoveFromSaveSet(display, w)
Display *display;
Window w;
{}

void XChangeSaveSet(display, w, change_mode)
Display *display;
Window w;
int change_mode;
{}


endexternal;


xlib_external_require XSaveSet;


global vars XSaveSet = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
