/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XWindowManipulation.p
 > Purpose:
 > Author:          Gareth Palmer,  3 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;


global constant macro (

/* ConfigureWindow structure */

 CWX                 = (1 << 0),
 CWY                 = (1 << 1),
 CWWidth             = (1 << 2),
 CWHeight            = (1 << 3),
 CWBorderWidth       = (1 << 4),
 CWSibling           = (1 << 5),
 CWStackMode         = (1 << 6),

/* Window stacking method (in configureWindow) */

 Above                   = 0,
 Below                   = 1,
 TopIf                   = 2,
 BottomIf                = 3,
 Opposite                = 4,

/* Circulation direction */

 RaiseLowest             = 0,
 LowerHighest            = 1,

);


external declare XWindowManipulation in c;
    (external_import_procedure XptImportProcedure)


    /*
     * Data structure for XReconfigureWindow
     */

    typedef struct {
        int x, y;
        int width, height;
        int border_width;
        Window sibling;
        int stack_mode;
    } XWindowChanges;


void XLowerWindow(display, w)
Display *display;
Window w;
{}

void XRaiseWindow(display, w)
Display *display;
Window w;
{}

void XCirculateSubwindows(display, w, direction)
Display *display;
Window w;
int direction;
{}

void XCirculateSubwindowsDown(display, w)
Display *display;
Window w;
{}

void XCirculateSubwindowsUp(display, w)
Display *display;
Window w;
{}

Status XQueryTree(display, w, root, parent, children, nchildren)
Display *display;
Window w;
Window *root;               ;;; RETURN
Window *parent;             ;;; RETURN
Window **children;          ;;; RETURN
unsigned int *nchildren;    ;;; RETURN
{}

void XReparentWindow(display, win, parent, x, y)
Display *display;
Window win;
Window parent;
int x, y;
{}

void XMoveWindow(display, w, x, y)
Display *display;
Window w;
int x, y;
{}

void XResizeWindow(display, w, width, height)
Display *display;
Window w;
unsigned int width, height;
{}

void XMoveResizeWindow(display, w, x, y, width, height)
Display *display;
Window w;
int x, y;
unsigned int width, height;
{}

void XSetWindowBorderWidth(display, w, width)
Display *display;
Window w;
unsigned int width;
{}

void XRestackWindows(display, windows, nwindows)
Display *display;
Window windows[];
int nwindows;
{}

void XConfigureWindow(display, w, value_mask, values)
Display *display;
Window w;
unsigned int value_mask;
XWindowChanges *values;
{}

endexternal;


xlib_external_require XWindowManipulation;


global vars XWindowManipulation = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
