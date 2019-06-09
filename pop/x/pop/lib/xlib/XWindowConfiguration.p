/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XWindowConfiguration.p
 > Purpose:
 > Author:          Gareth Palmer, 3 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XWindowManipulation;


external declare XWindowConfiguration in c;
    (external_import_procedure XptImportProcedure)

;;; duplicated from WindowAttributes
Status XGetGeometry(display, drawable, root, x, y, width, height,
                      border_width, depth)
Display *display;
Drawable drawable;
Window *root;                    ;;; RETURN
int *x, *y;                      ;;; RETURN
unsigned int *width, *height;    ;;; RETURN
unsigned int *border_width;      ;;; RETURN
unsigned int *depth;             ;;; RETURN
{}

endexternal;


xlib_external_require XWindowConfiguration;


global vars XWindowConfiguration = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
