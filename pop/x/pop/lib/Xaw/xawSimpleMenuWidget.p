/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xaw/xawSimpleMenuWidget.p
 > Purpose:         Athena widgetclass and associated procedures
 > Author:          John Gibson, Apr 17 1993
 > Documentation:   REF * ATHENA
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include xpt_coretypes.ph;

XptLoadWidgetClass xawSimpleMenuWidget [^^XAW_EXLIBS]
    xawSimpleMenuWidget <- simpleMenuWidgetClass,
;

define XawIsSimpleMenuWidget =
    XtIsSubclass(% xawSimpleMenuWidget %)
enddefine;

XptPopLoadProcedures ''
    XawSimpleMenuAddGlobalActions(x) :void,
    XawSimpleMenuGetActiveEntry(x) :XptWidget,
    XawSimpleMenuClearActiveEntry(x) :void,
;

endexload_batch;
endsection;
