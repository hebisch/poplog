/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/DefaultScreen.p
 > Purpose:         Default screen of a display
 > Author:          Adrian Howard, Jun 30 1993
 > Documentation:   * DefaultScreen
 > Related Files:   LIB * XlibMacros, LIB * XHouseKeeping
 */

compile_mode: pop11 +strict;
section;

/*
 * DefaultScreen is defined in LIB * XlibMacros. This autoloadable was
 * installed to keep backwards compatability with LIB * XHouseKeeping
 * which previously duplicated the code of DefaultScreen.
 */

uses XlibMacros;

endsection;
