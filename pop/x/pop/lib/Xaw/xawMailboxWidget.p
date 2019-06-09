/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xaw/xawMailboxWidget.p
 > Purpose:         Athena widgetclass and associated procedures
 > Author:          John Gibson, Apr 17 1993
 > Documentation:   REF * ATHENA
 */
compile_mode :pop11 +strict;

section;

XptLoadWidgetClass xawMailboxWidget [^^XAW_EXLIBS]
    xawMailboxWidget    <- mailboxWidgetClass,
;

define XawIsMailboxWidget =
    XtIsSubclass(% xawMailboxWidget %)
enddefine;

endsection;
