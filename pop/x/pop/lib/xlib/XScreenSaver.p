/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XScreenSaver.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;

global constant macro (

 DontPreferBlanking  = 0,
 PreferBlanking      = 1,
 DefaultBlanking     = 2,

 DisableScreenSaver      = 0,
 DisableScreenInterval   = 0,

 DontAllowExposures  = 0,
 AllowExposures      = 1,
 DefaultExposures    = 2,

/* for ForceScreenSaver */

 ScreenSaverReset = 0,
 ScreenSaverActive = 1,

);


external declare XScreenSaver in c;
    (external_import_procedure XptImportProcedure)

void XActivateScreenSaver(display)
Display *display;
{}

void XForceScreenSaver(display, mode)
Display *display;
int mode;
{}

void XResetScreenSaver(display)
Display *display;
{}

void XGetScreenSaver(display, timeout, interval, prefer_blanking,
                        allow_exposures)
Display *display;
int *timeout, *interval;        ;;; RETURN
int *prefer_blanking;           ;;; RETURN
int *allow_exposures;           ;;; RETURN
{}

void XSetScreenSaver(display, timeout, interval, prefer_blanking,
                        allow_exposures)
Display *display;
int timeout, interval;
int prefer_blanking;
int allow_exposures;
{}


endexternal;


xlib_external_require XScreenSaver;


global vars XScreenSaver = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
