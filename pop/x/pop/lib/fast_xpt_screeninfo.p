/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xpt_screeninfo.p
 > Purpose:         Accessing information in screens
 > Author:          Adrian Howard, Jun 30 1993 (see revisions)
 > Documentation:   * XDisplayOfScreen
 > Related Files:   LIB * XlibMacros, LIB *XPT_SCREENINFO
 >                  LIB * XptScreenPtrApply
 */

compile_mode: pop11 +strict;
section;

/*
 * The code here was extracted from LIB * XPT_SCREENINFO to avoid
 * code duplication in LIB *XlibMacros.
 *
 * It was also altered to use procedure calls rather than directly
 * access the opaque screen structure
 */

include xpt_coretypes;
include xpt_xtypes;

XptLoadProcedures fast_xpt_screeninfo
    lvars
        XDisplayOfScreen,
        XRootWindowOfScreen,
        XBlackPixelOfScreen,
        XWhitePixelOfScreen,
        XDefaultGCOfScreen,
        XPlanesOfScreen,
        XWidthOfScreen,
        XHeightOfScreen,
        XWidthMMOfScreen,
        XHeightMMOfScreen,
        XCellsOfScreen,
        XMinCmapsOfScreen,
        XMaxCmapsOfScreen,
        XDoesSaveUnders,
        XDoesBackingStore,
        XEventMaskOfScreen,
        XDefaultColormapOfScreen,
        XDefaultDepthOfScreen,
        XDefaultVisualOfScreen,
        XScreenNumberOfScreen,
;

;;; RETURNS THE WIDTH OF THE SPECIFIED SCREEN
define fast_XWidthOfScreen() with_nargs 1;
    exacc (1):int raw_XWidthOfScreen();
enddefine;

;;; RETURNS THE HEIGHT OF THE SPECIFIED SCREEN
define fast_XHeightOfScreen() with_nargs 1;
    exacc (1):int raw_XHeightOfScreen();
enddefine;

;;; RETURNS THE WIDTH OF THE SPECIFIED SCREEN
define fast_XWidthMMOfScreen() with_nargs 1;
    exacc (1):int raw_XWidthMMOfScreen();
enddefine;

;;; RETURNS THE HEIGHT OF THE SPECIFIED SCREEN
define fast_XHeightMMOfScreen() with_nargs 1;
    exacc (1):int raw_XHeightMMOfScreen();
enddefine;

;;; RETURN THE NUMBER OF PLANES ON THE SPECIFIED SCREEN
define fast_XPlanesOfScreen() with_nargs 1;
    exacc (1):int raw_XPlanesOfScreen();
enddefine;

;;; RETURNS THE ROOT WINDOW ASSOCIATED WITH THE SPECIFIED SCREEN.
define fast_XRootWindowOfScreen() with_nargs 1;
    exacc (1):XptWindow raw_XRootWindowOfScreen();
enddefine;

;;; RETURNS THE DEFAULT GC OF A SCREEN
define fast_XDefaultGCOfScreen() with_nargs 1;
    exacc (1):XptGC raw_XDefaultGCOfScreen();
enddefine;

;;; RETURNS THE BLACK PIXEL ASSOCIATED WITH THE SPECIFIED SCREEN.
define fast_XBlackPixelOfScreen() with_nargs 1;
    exacc (1):ulong raw_XBlackPixelOfScreen();
enddefine;

;;; RETURNS THE WHITE PIXEL ASSOCIATED WITH THE SPECIFIED SCREEN.
define fast_XWhitePixelOfScreen() with_nargs 1;
    exacc (1):ulong raw_XWhitePixelOfScreen();
enddefine;

;;; RETURNS THE DISPLAY ASSOCIATED WITH THE SPECIFIED SCREEN.
define fast_XDisplayOfScreen() with_nargs 1;
    exacc (1):XptDisplayPtr raw_XDisplayOfScreen();
enddefine;

;;; NUMBER OF COLOR CELLS IN DEFAULT COLORMAP OF SCREEN
define fast_XCellsOfScreen() with_nargs 1;
    exacc (1):int raw_XCellsOfScreen();
enddefine;

;;; MIN NUMBER OF INSTALLED COLORMAPS FOR THE SCREEN
define fast_XMinCmapsOfScreen() with_nargs 1;
    exacc (1):int raw_XMinCmapsOfScreen();
enddefine;

;;; MAX NUMBER OF INSTALLED COLORMAPS FOR THE SCREEN
define fast_XMaxCmapsOfScreen() with_nargs 1;
    exacc (1):int raw_XMaxCmapsOfScreen();
enddefine;

;;; TRUE IF SCREEN SUPPORTS "SAVE UNDERS"
define fast_XDoesSaveUnders() with_nargs 1;
    exacc (1):XptBoolean raw_XDoesSaveUnders();
enddefine;

;;; DOES THE SCREEN SUPPORT BACKING STORE?
define fast_XDoesBackingStore() with_nargs 1;
    exacc (1):int raw_XDoesBackingStore();
enddefine;

;;; EVENT MASK OF SCREENS ROOT WINDOW
define fast_XEventMaskOfScreen() with_nargs 1;
    exacc (1):long raw_XEventMaskOfScreen();
enddefine;

;;; DEFAULT COLORMAP OF SPECIFIED SCREEN
define fast_XDefaultColormapOfScreen() with_nargs 1;
    exacc (1):XptColormap raw_XDefaultColormapOfScreen();
enddefine;

;;; DEFAULT VISUAL OF SPECIFIED SCREEN
define fast_XDefaultVisualOfScreen() with_nargs 1;
    exacc (1):XptVisual raw_XDefaultVisualOfScreen();
enddefine;

;;; DEFAULT DEPTH OF SPECIFIED SCREEN
define fast_XDefaultDepthOfScreen = fast_XPlanesOfScreen enddefine;

;;; THE NUMBER OF THE SPECIFIED SCREEN
define fast_XScreenNumberOfScreen() with_nargs 1;
    exacc (1):int raw_XScreenNumberOfScreen();
enddefine;

constant fast_xpt_screeninfo = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jul  5 1993
        Added fast_XScreenNumberOfScreen, fast_XDefaultVisualOfScreen and
        fast_XDefaultDepthOfScreen
--- Adrian Howard, Jul  4 1993
        Added fast_XDefaultColormapOfScreen
--- Adrian Howard, Jun 30 1993
        Added fast_XCellsOfScreen, fast_XMinCmapsOfScreen,
        fast_XMaxCmapsOfScreen, fast_XDoesSaveUnders,
        fast_XDoesBackingStore, and fast_XEventMaskOfScreen
 */
