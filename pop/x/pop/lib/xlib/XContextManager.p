/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XContextManager.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */

uses XConstants;

global constant macro(

XCSUCCESS = 0,    /* No error. */
XCNOMEM   = 1,    /* Out of memory */
XCNOENT   = 2,    /* No entry in table */

);

external declare XContextManager in c;
    (external_import_procedure XptImportProcedure)


typedef int XContext;


int XDeleteContext(display, w, context)
Display *display;
Window w;
XContext context;
{}

int XFindContext(display, w, context, data)
Display *display;
Window w;
XContext context;
caddr_t *data;                  ;;; RETURN
{}

int XSaveContext(display, w, context, data)
Display *display;
Window w;
XContext context;
caddr_t data;
{}

;;;XContext XUniqueContext()            Undefined:
;;;{}                                   _XUniqueContext

endexternal;

xlib_external_require XContextManager;

global vars XContextManager = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to us xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
