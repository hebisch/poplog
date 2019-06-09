/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/WhitePixel.p
 > Purpose:         White pixel of a display
 > Author:          Adrian Howard, Jun 10 1993
 > Documentation:   * WhitePixel
 > Related Files:   XlibMacros.p XColorCells.p BlackPixel.p
 */

compile_mode: pop11 +strict;
section;

/*
 * WhitePixel is defined in LIB * XlibMacros. This autoloadable was
 * installed to keep backwards compatability with LIB * XColorcells
 * which previously duplicated the code of WhitePixel.
 */

uses XlibMacros;

endsection;
