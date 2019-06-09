/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/DefaultVisual.p
 > Purpose:         Default visual of screen
 > Author:          Adrian Howard, Jun 21 1993
 > Documentation:   * DefaultVisual
 > Related Files:   LIB * XlibMacros, LIB * XColormaps
 */

compile_mode: pop11 +strict;
section;

/*
 * DefaultVisual is defined in LIB * XlibMacros. This autoloadable was
 * installed to keep backwards compatability with LIB * XColormaps
 * which previously duplicated the code of DefaultVisual.
 */

uses XlibMacros;

endsection;
