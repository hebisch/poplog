/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/BlackPixel.p
 > Purpose:         Get black pixel of a display
 > Author:          Adrian Howard, Jun 10 1993
 > Documentation:   * BlackPixel
 > Related Files:   XlibMacros.p XColorCells.p WhitePixel.p
 */

compile_mode: pop11 +strict;
section;

/*
 * BlackPixel is defined in LIB * XlibMacros. This autoloadable was
 * installed to keep backwards compatability with LIB * XColorcells
 * which previously duplicated the code of BlackPixel.
 */

uses XlibMacros;

endsection;
