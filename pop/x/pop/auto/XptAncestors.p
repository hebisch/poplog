/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptAncestors.p
 > Purpose:         Returns all parent widgets up to shell widget
 > Author:          Jonathan Meyer, Jul  5 1991
 > Documentation:   REF *XT_LIBS/XptAncestors
 > Related Files:   LIB *XptChildren
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses fast_xt_widgetclass;
uses fast_xt_widgetinfo;

define XptAncestors(widget);
    lvars widget;
    XptCheckWidget(widget)->;
    fast_ncrev([%
        until fast_XtIsShell(widget) do
            widget;
            fast_XtParent(widget) -> widget;
        enduntil;
        widget;
    %]);
enddefine;

endexload_batch;
endsection;
