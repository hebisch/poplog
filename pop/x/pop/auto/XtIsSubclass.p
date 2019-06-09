/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XtIsSubclass.p
 > Purpose:         Test if widget is a subclass of given class
 > Author:          John Gibson, Apr 13 1993
 */
compile_mode :pop11 +strict;

section;
exload_batch;

;;; Returns true if widget is a subclass of the given WidgetClass - 17/07/90
;;; Input - <Widget> <WidgetClass>, Output - <BOOL>

define XtIsSubclass(widget, class);
    lvars widget, class;
    fast_XtIsSubclass(XptCheckWidget(widget), XptCheckWidgetClass(class));
enddefine;

endexload_batch;
endsection;
