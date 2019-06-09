/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptGarbageCursorFeedback.p
 > Purpose:         Change cursor during garbage collection
 > Author:          Jonathan Meyer, Sep  1 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;
section;

uses xpt_clientcallback;
uses xpt_cursorplane;

lconstant
    macro (DPY = "fast_front", CURSOR = "fast_back"),
    GC_CB_NAME = 'garbageCollectorChangeState', ;;; private cb list name
    gc_widgets = writeable newproperty([],10,false,"tmparg");

define lconstant gc_cb(w, client, call);
    lvars w, client, call, new;
    if call then
        ;;; install new cursor - save old cursor
        XptGarbageCursor(DPY(client)) -> new;
        XptNewCursorPlaneCursor(w, new) -> CURSOR(client);
    else
        XptNewCursorPlaneCursor(w, CURSOR(client))->;
    endif;
enddefine;

define global XptGarbageCursorFeedback(widget);
    lvars widget;
    gc_widgets(XptCheckWidget(widget)) and true;
enddefine;

define updaterof XptGarbageCursorFeedback(enabled, widget);
    lvars enabled, widget, pair;
    gc_widgets(XptCheckWidget(widget))->pair;
    if enabled and not(pair) then
        ;;; setup for new widget
        true -> XptCursorPlaneOn(widget);
        XptGarbageCursor(fast_XtDisplay(widget))->; ;;; get the gc cursor
        conspair(fast_XtDisplay(widget), false) ->> gc_widgets(widget) -> pair;
        XptAddClientCallback(widget, GC_CB_NAME, gc_cb, pair);
    elseif not(enabled) and pair then
        false -> gc_widgets(widget);
        XptRemoveClientCallback(widget, GC_CB_NAME, gc_cb, pair);
        sys_grbg_list(pair);
    endif;
enddefine;


sysunprotect("XptGarbageHook");

procedure(why) -> why;
    lvars why, dpy;
    dlocal popmemlim = 2**28, XptTraceCallback=false, XptApplyCallback=false;
    XptCallAllClientCallbacks(GC_CB_NAME, why);
endprocedure,
if XptGarbageHook /== identfn then
    <> XptGarbageHook
endif -> XptGarbageHook;

sysprotect("XptGarbageHook");

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jul  7 1993
        Switched of tracing during call to gc_cb since user-defined trace
        procedure could allocate memory which would foul up XptGarbageHandler
 */
