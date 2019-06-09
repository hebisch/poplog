/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XSelections.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;

external declare XSelections in c;
    (external_import_procedure XptImportProcedure)


Window XGetSelectionOwner(display, selection)
Display *display;
Atom selection;
{}

void XSetSelectionOwner(display, selection, owner, time)
Display *display;
Atom selection;
Window owner;
Time time;
{}

void XConvertSelection(display, selection, target, property, requestor, time)
Display *display;
Atom selection, target;
Atom property;              ;;; may be None
Window requestor;
Time time;
{}


endexternal;


xlib_external_require XSelections;

global vars XSelections = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use  xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
