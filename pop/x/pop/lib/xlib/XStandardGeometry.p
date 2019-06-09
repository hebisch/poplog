/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XStandardGeometry.p
 > Purpose:
 > Author:          Gareth Palmer,  3 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;

global constant macro(

/*
 * Bitmask returned by XParseGeometry().  Each bit tells if the corresponding
 * value (x, y, width, height) was found in the parsed string.
 */
NoValue     = 16:0000,
XValue      = 16:0001,
YValue      = 16:0002,
WidthValue  = 16:0004,
HeightValue = 16:0008,
AllValues   = 16:000F,
XNegative   = 16:0010,
YNegative   = 16:0020,

);

external declare XStandardGeometry in c;
    (external_import_procedure XptImportProcedure)


int XGeometry(display, screen, user_geom, default_geom, bwidth, fwidth,
                fheight, xadder, yadder, x, y, width, height)
Display *display;
int screen;
char *user_geom, *default_geom;
unsigned int bwidth;
unsigned int fwidth, fheight;
int xadder, yadder;
int *x, *y, *width, *height;    ;;; RETURN
{}

int XParseGeometry(parsestring, x, y, width, height)
char *parsestring;
int *x, *y;                     ;;; RETURN                   
unsigned int *width, *height;   ;;; RETURN
{}

Bool XTranslateCoordinates(display, src_w, dest_w, src_x, src_y, dest_x,
                            dest_y, child)
Display *display;
Window src_w, dest_w;
int src_x, src_y;
int *dest_x, *dest_y;          ;;; RETURN
Window *child;                 ;;; RETURN                   
{}


endexternal;


xlib_external_require XStandardGeometry;


global vars XStandardGeometry = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
