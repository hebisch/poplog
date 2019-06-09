/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptShellOfObject.p
 > Purpose:         Ascends widget tree looking for Shell subclass
 > Author:          Jonathan Meyer, Jul  5 1991
 > Documentation:   REF *XptShellOfObject
 > Related Files:   REF *XptAncestors
 */

compile_mode :pop11 +strict;

uses fast_xt_widgetclass;
uses fast_xt_widgetinfo;

section;

/* Returns first Shell ancester */

define global XptShellOfObject(widget) -> widget;
    lvars widget;
    XptCheckWidget(widget)->;
    while widget and not(fast_XtIsShell(widget)) do
        fast_XtParent(widget) -> widget;
    endwhile;
enddefine;

endsection;
