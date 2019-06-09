/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xaw/xawSmeObject.p
 > Purpose:         Athena widgetclass and associated procedures
 > Author:          John Gibson, Apr 17 1993
 > Documentation:   REF * ATHENA
 */
compile_mode :pop11 +strict;

section;

XptLoadWidgetClass xawSmeObject [^^XAW_EXLIBS]
    xawSmeObject    <- smeObjectClass,
;

define XawIsSmeObject =
    XtIsSubclass(% xawSmeObject %)
enddefine;

endsection;
