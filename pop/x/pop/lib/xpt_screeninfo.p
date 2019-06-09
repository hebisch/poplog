/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xpt_screeninfo.p
 > Purpose:         Routines for extracting information about a screen
 > Author:          Jonathan Meyer, Nov 3 1990 (see revisions)
 > Documentation:   REF *XPT_SCREENINFO
 */
compile_mode :pop11 +strict;

section;

uses xpt_typecheck;
uses fast_xpt_screeninfo;

define XDisplayOfScreen() with_nargs 1;
    fast_XDisplayOfScreen(XptCheckScreenPtr());
enddefine;

define XRootWindowOfScreen() with_nargs 1;
    fast_XRootWindowOfScreen(XptCheckScreenPtr());
enddefine;

define XBlackPixelOfScreen() with_nargs 1;
    fast_XBlackPixelOfScreen(XptCheckScreenPtr());
enddefine;

define XWhitePixelOfScreen() with_nargs 1;
    fast_XWhitePixelOfScreen(XptCheckScreenPtr());
enddefine;

define XDefaultGCOfScreen() with_nargs 1;
    fast_XDefaultGCOfScreen(XptCheckScreenPtr());
enddefine;

define XPlanesOfScreen() with_nargs 1;
    fast_XPlanesOfScreen(XptCheckScreenPtr());
enddefine;

define XWidthOfScreen() with_nargs 1;
    fast_XWidthOfScreen(XptCheckScreenPtr());
enddefine;

define XHeightOfScreen() with_nargs 1;
    fast_XHeightOfScreen(XptCheckScreenPtr());
enddefine;

define XWidthMMOfScreen() with_nargs 1;
    fast_XWidthMMOfScreen(XptCheckScreenPtr());
enddefine;

define XHeightMMOfScreen() with_nargs 1;
    fast_XHeightMMOfScreen(XptCheckScreenPtr());
enddefine;

define XCellsOfScreen() with_nargs 1;
    fast_XCellsOfScreen(XptCheckScreenPtr());
enddefine;

define XMinCmapsOfScreen() with_nargs 1;
    fast_XMinCmapsOfScreen(XptCheckScreenPtr());
enddefine;

define XMaxCmapsOfScreen() with_nargs 1;
    fast_XMaxCmapsOfScreen(XptCheckScreenPtr());
enddefine;

define XDoesSaveUnders() with_nargs 1;
    fast_XDoesSaveUnders(XptCheckScreenPtr());
enddefine;

define XDoesBackingStore() with_nargs 1;
    fast_XDoesBackingStore(XptCheckScreenPtr());
enddefine;

define XEventMaskOfScreen() with_nargs 1;
    fast_XEventMaskOfScreen(XptCheckScreenPtr());
enddefine;

define XDefaultColormapOfScreen() with_nargs 1;
    fast_XDefaultColormapOfScreen(XptCheckScreenPtr());
enddefine;

define XDefaultVisualOfScreen() with_nargs 1;
    fast_XDefaultVisualOfScreen(XptCheckScreenPtr());
enddefine;

define XDefaultDepthOfScreen = XPlanesOfScreen enddefine;

define XScreenNumberOfScreen() with_nargs 1;
    fast_XScreenNumberOfScreen(XptCheckScreenPtr());
enddefine;

global constant xpt_screeninfo = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jul  5 1993
        Added XScreenNumberOfScreen, XDefaultVisualOfScreen and
        XDefaultDepthOfScreen
--- Adrian Howard, Jul  4 1993
        Added XDefaultColormapOfScreen
--- Adrian Howard, Jun 30 1993
     #  Added XCellsOfScreen, XMinCmapsOfScreen, XMaxCmapsOfScreen,
        XDoesSaveUnders, XDoesBackingStore, XEventMaskOfScreen
     #  Replaced XptCheckScreen with XptCheckScreenPtr
     #  Created "fast" versions of the procedures in LIB * FAST_XPT_SCREENINFO
--- Jonathan Meyer, Nov 23 1990
        Added include xpt_xscreen and uses XptImportScreenPtr
--- Roger Evans, Nov 19 1990 made procedures global
 */
