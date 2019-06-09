/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XlibMacros.p
 > Purpose:         Pop replacements for Xlib macros
 > Author:          Gareth Palmer, Sep  5 1989 (see revisions)
 > Documentation:   REF *XlibMacros
 > Related Files:
 */

compile_mode: pop11 +strict;
section;

include xpt_xtypes;
uses XConstants;
uses fast_xpt_screeninfo;

external declare XlibMacros in c;
    (external_import_procedure XptImportProcedure)

    int XConnectionNumber(display)
        Display *display;
    {}

    Window XRootWindow(display, screen_number)
        Display *display;
        int screen_number;
    {}

    unsigned long XBlackPixel(display, screen_number)
        Display *display;
        int screen_number;
    {}

    unsigned long XWhitePixel(display, screen_number)
        Display *display;
        int screen_number;
    {}

    Colormap XDefaultColormap(display, screen_number)
        Display *display;
        int screen_number;
    {}

    int XDisplayCells(display, screen_number)
        Display *display;
        int screen_number;
    {}

    Visual XDefaultVisual(display, screen_number)
        Display *display;
        int screen_number;
    {}

    Window XDefaultRootWindow(display)
        Display *display;
    {}

    GC XDefaultGC(display, screen_number)
        Display *display;
        int screen_number;
    {}

    unsigned long XAllPlanes() {}

    int XQLength(display)
        Display *display;
    {}

    int XDisplayWidth(display, screen_number)
        Display *display;
        int screen_number;
    {}

    int XDisplayHeight(display, screen_number)
        Display *display;
        int screen_number;
    {}

    int XDisplayWidthMM(display, screen_number)
        Display *display;
        int screen_number;
    {}

    int XDisplayHeightMM(display, screen_number)
        Display *display;
        int screen_number;
    {}

    int XScreenCount(display)
        Display *display;
    {}

    char *XServerVendor(display)
        Display *display;
    {}

    int XProtocolVersion(display)
        Display *display;
    {}

    int XProtocolRevision(display)
        Display *display;
    {}

    int XVendorRelease(display)
        Display *display;
    {}

    char *XDisplayString(display)
        Display *display;
    {}

    int XDefaultDepth(display, screen_number)
        Display *display;
        int screen_number;
    {}

    unsigned long XNextRequest(display)
        Display *display;
    {}

    unsigned long XLastKnownRequestProcessed(display)
        Display *display;
    {}

    Screen *XScreenOfDisplay(display, screen_number)
        Display *display;
        int screen_number;
    {}

    Screen *XDefaultScreenOfDisplay(display)
        Display *display;
    {}

    int XBitmapBitOrder(display)
        Display *display;
    {}

    int XBitmapPad(display)
        Display *display;
    {}

    int XBitmapUnit(display)
        Display *display;
    {}

    int XImageByteOrder(display)
        Display *display;
    {}

    int XDefaultScreen(display)
        Display *display;
    {}

endexternal;

xlib_external_require XlibMacros;

;;; SO WE GET POP-11 STRINGS RATHER THAN POINTERS
XServerVendor <> exacc_ntstring -> XServerVendor;
XDisplayString <> exacc_ntstring -> XDisplayString;

;;; THEY DO EXACTLY THE SAME THING SO THERE'S NO POINT IN LOADING
;;; THE EXTERNAL VERSION.
define XDisplayPlanes = XDefaultDepth enddefine;

define ConnectionNumber = XConnectionNumber enddefine;
define RootWindow = XRootWindow enddefine;
define DefaultRootWindow = XDefaultRootWindow enddefine;
define DefaultGC = XDefaultGC enddefine;
define QLength = XQLength enddefine;
define DisplayWidth = XDisplayWidth enddefine;
define DisplayHeight = XDisplayHeight enddefine;
define DisplayWidthMM = XDisplayWidthMM enddefine;
define DisplayHeightMM = XDisplayHeightMM enddefine;
define DisplayPlanes = XDisplayPlanes enddefine;
define ScreenCount = XScreenCount enddefine;
define ServerVendor = XServerVendor enddefine;
define ProtocolVersion = XProtocolVersion enddefine;
define ProtocolRevision = XProtocolRevision enddefine;
define VendorRelease = XVendorRelease enddefine;
define DisplayString = XDisplayString enddefine;
define DefaultDepth = XDefaultDepth enddefine;
define NextRequest = XNextRequest enddefine;
define LastKnownRequestProcessed = XLastKnownRequestProcessed enddefine;
define ScreenOfDisplay = XScreenOfDisplay enddefine;
define DefaultScreenOfDisplay = XDefaultScreenOfDisplay enddefine;
define BitmapBitOrder = XBitmapBitOrder enddefine;
define BitmapPad = XBitmapPad enddefine;
define BitmapUnit = XBitmapUnit enddefine;
define ImageByteOrder = XImageByteOrder enddefine;
define DefaultScreen = XDefaultScreen enddefine;
define DefaultColormap = XDefaultColormap enddefine;
define DefaultVisual = XDefaultVisual enddefine;
define DisplayCells = XDisplayCells enddefine;
define BlackPixel = XBlackPixel enddefine;
define WhitePixel = XWhitePixel enddefine;

;;; This is duplicated by include XpwPixmap.ph
#_IF not(DEF AllPlanes)
    constant macro AllPlanes = -1;
#_ENDIF

;;; THESE ARE DEFINED IN LIB * FAST_XPT_SCREENINFO
define DisplayOfScreen = fast_XDisplayOfScreen enddefine;
define RootWindowOfScreen = fast_XRootWindowOfScreen enddefine;
define BlackPixelOfScreen = fast_XBlackPixelOfScreen enddefine;
define WhitePixelOfScreen = fast_XWhitePixelOfScreen enddefine;
define DefaultGCOfScreen = fast_XDefaultGCOfScreen enddefine;
define WidthOfScreen = fast_XWidthOfScreen enddefine;
define HeightOfScreen = fast_XHeightOfScreen enddefine;
define WidthMMOfScreen = fast_XWidthMMOfScreen enddefine;
define HeightMMOfScreen = fast_XHeightMMOfScreen enddefine;
define PlanesOfScreen = fast_XPlanesOfScreen enddefine;
define CellsOfScreen = fast_XCellsOfScreen enddefine;
define MinCmapsOfScreen = fast_XMinCmapsOfScreen enddefine;
define MaxCmapsOfScreen = fast_XMaxCmapsOfScreen enddefine;
define DoesSaveUnders = fast_XDoesSaveUnders enddefine;
define DoesBackingStore = fast_XDoesBackingStore enddefine;
define EventMaskOfScreen = fast_XEventMaskOfScreen enddefine;
define DefaultDepthOfScreen = fast_XDefaultDepthOfScreen enddefine;
define DefaultVisualOfScreen = fast_XDefaultVisualOfScreen enddefine;

/*
 * #define XAllocID(dpy) ((*(dpy)->resource_alloc)((dpy)))
 * Allocates and returns a resource ID --- unfortunately there is no
 * functional equivalent of this macro in R4 Xlib so we'll have to be a
 * bit naughty and access the display structure directly
 */
define XAllocID(dpy);
    lvars dpy;
    exacc (1):XptXID (exacc :Display dpy.resource_alloc)(dpy);
enddefine;

constant XlibMacros = true;

endsection


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 31 1995
        Made AllPlanes a constant macro so it's exactly the same as
        loadinclude XpwPixmap.ph will give, and guarded it with #_IF
--- John Gibson, Apr 27 1994
        Changed definition of AllPlanes to constant -1 so it doesn't
        conflict with the one in include XpwPixmap.ph
--- Adrian Howard, Jul  5 1993
     #  Re-wrote XAllocID so it works.
     #  XServerVendor and XDisplayString now return Pop-11 strings
     #  XDefaultDepthOfScreen and XDefaultVisualOfScreen now in
        LIB * XPT_SCREENINFO
--- Adrian Howard, Jul  4 1993
     #  Tidied, sectioned, made +strict
     #  Uses function calls instead of (illegal) direct access to Display
        structure where possible.
     #  Uses LIB * FAST_XPT_SCREENINFO instead of duplicating code
     #  Added XConnectionNumber, XRootWindow, XDefaultRootWindow, XDefaultGC,
        XAllPlanes, XQLength, XDisplayWidth, XDisplayHeight, XDisplayWidthMM,
        XDisplayHeightMM, XDisplayPlanes, XScreenCount, XServerVendor,
        XProtocolVersion, XProtocolRevision, XVendorRelease, XDisplayString,
        XDefaultDepth, XNextRequest, XLastKnownRequestProcessed, XScreenOfDisplay,
        XDefaultScreenOfDisplay, XDefaultDepthOfScreen, XDefaultVisualOfScreen,
        XBitmapBitOrder, XBitmapPad, XBitmapUnit, XDefaultScreen,
        XDefaultColormap, XDefaultVisual, XDisplayCells, XImageByteOrder,
        XBlackPixel, and XWhitePixel.
     #  Made DefaultScreen autoloadable to avoid code duplication
--- Adrian Howard, Jun 21 1993
        Made DefaultColormap, DefaultVisual, DefaultColormapOfScreen, and
        DisplayCells autoloadable to avoid code duplication in LIB * XColormaps
--- Adrian Howard, Jun 10 1993
        Made BlackPixel and WhitePixel autoloadable to avoid their duplication
        in LIB * XColorCells
 */
