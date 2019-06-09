/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/load_xlib.p
 > Purpose:
 > Author:          Ian Rogers, 5th July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


max(1000000, popmemlim) -> popmemlim;
include xdefs.ph;

uses XlibMacros;

#_IF DEF XHASOLDX
uses XAssociationTables;
#_ENDIF

uses XBuffers;
uses XClientConnections;
uses XColorcells;
uses XColormaps;
uses XContextManager;
uses XCursors;
uses XDrawingPrimitives;
uses XErrors;
uses XEvents;
uses XExtensions;
uses XFonts;
uses XGrabbing;
uses XGraphicsContext;
uses XHostAccess;
uses XHouseKeeping;
uses XImages;
uses XKeyboard;
uses XPointers;
uses XProperties;
uses XRegions;
uses XSaveSet;
uses XScreenSaver;
uses XSelections;
uses XStandardGeometry;
uses XText;
uses XTile;
uses XUserPreferences;
uses XVisuals;
uses XWindowAttributes;
uses XWindowConfiguration;
uses XWindowExistence;
uses XWindowManager;
uses XWindowManipulation;
uses XWindowMapping;

external_require_load(); ;;; flush all the require's

global constant load_xlib = true;

/* --- Revision History ---------------------------------------------------
--- Simon Nichols, Oct 29 1991
        Changed test from #_IF XHASOLDX to #_IF DEF XHASOLDX as XHASOLDX
        may not be defined. See bugreport ianr.27.
--- Jonathan Meyer, Jan 25 1991 Added guard on XAssociationTables
 */
