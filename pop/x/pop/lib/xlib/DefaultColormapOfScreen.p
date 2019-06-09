/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/DefaultColormapOfScreen.p
 > Purpose:         Default colormap of a screen
 > Author:          Adrian Howard, Jun 21 1993 (see revisions)
 > Documentation:   * DefaultColormapOfScreen
 > Related Files:   LIB * XPT_SCREENINFO, LIB * XColormaps,
 >                  LIB * XlibMacros
 */

compile_mode: pop11 +strict;
section;

uses fast_xpt_screeninfo;

/*
 * Moved from LIB * XlibMacros and LIB * XColormaps to avoid code
 * duplication
 */

define DefaultColormapOfScreen = fast_XDefaultColormapOfScreen enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jul  4 1993
        Now uses LIB * FAST_XPT_SCREENINFO
 */
