/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XtDisplay.p
 > Purpose:         Get widget's display
 > Author:          John Gibson, Apr 16 1993
 > Documentation:   REF * XT_DISPLAY
 */
compile_mode :pop11 +strict;

section;

;;; Returns the display pointer for the specified widget - 17/07/90
;;; Input - <Widget>, Output - <DisplayPtr>
define XtDisplay() with_nargs 1;
    fast_XtDisplay(XptCheckWidget())
enddefine;

endsection;
