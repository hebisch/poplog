/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XErrors.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;

external declare XErrors in c;
    (external_import_procedure XptImportProcedure)

void XGetErrorDatabaseText(display, name, message, default_string, buffer,
                            length)
Display *display;
char *name, *message;
char *default_string;
char *buffer;                   ;;; RETURN
int length;
{}

void XGetErrorText(display, code, buffer, length)
Display *display;
int code;
char *buffer;               ;;; RETURN
int length;
{}

void XSetErrorHandler(handler)
 ;;;int (* handler)(Display *, XErrorEvent *);
int (*handler)();
{}

void XSetIOErrorHandler(handler)
 ;;;int (* handler)(Display *);
int (*handler)();
{}

char *XDisplayName(string)
char *string;
{}

;;; int (*XSetAfterFunction(display, func))()
int *XSetAfterFunction(display, func)
Display *display;
int (*func)();
{}

;;; duplicated from Events
;;; int (*XSynchronize(display, onoff))()
int *XSynchronize(display, onoff)
Display *display;
int onoff;
{}


endexternal;


xlib_external_require XErrors;


global vars XErrors = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
