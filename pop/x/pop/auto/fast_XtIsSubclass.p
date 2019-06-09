/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/auto/fast_XtIsSubclass.p
 > Purpose:         Test if widget is a subclass of given class
 > Author:          John Gibson, Apr 13 1993
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include xpt_coretypes.ph;

;;; Returns true if widget is a subclass of the given WidgetClass - 17/07/90
;;; Input - <Widget> <WidgetClass>, Output - <BOOL>

XptLoadProcedures fast_XtIsSubclass
lvars XtIsSubclass(w,wc) :XptBoolean;

define fast_XtIsSubclass() with_nargs 2;
    exacc raw_XtIsSubclass();
enddefine;

endexload_batch;
endsection;
