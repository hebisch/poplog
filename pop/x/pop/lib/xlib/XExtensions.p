/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XExtensions.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;

external declare XExtensions in c;
    (external_import_procedure XptImportProcedure)


void XFreeExtensionList(list)
char **list;
{}

char **XListExtensions(display, nextensions)
Display *display;
int *nextensions;               ;;; RETURN
{}

Bool XQueryExtension(display, name, major_opcode, first_event, first_error)
Display *display;
char *name;
int *major_opcode;              ;;; RETURN
int *first_event;               ;;; RETURN
int *first_error;               ;;; RETURN
{}


endexternal;


xlib_external_require XExtensions;


global vars XExtensions = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
