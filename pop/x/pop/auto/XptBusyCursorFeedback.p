/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptBusyCursorFeedback.p
 > Purpose:         Install/uninstall a busy cursor
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
    BUSY_CB_NAME = 'clientBusy',    ;;; a private name (since its a string)
    REFRESH = 'refresh',            ;;; used when the busy cursor changes
    busy_widgets = writeable newproperty([],10,false,"tmparg");

lvars
    new_cursor,
    new_dpy,
    client_busy = false,
    trap_assigned = false,
;

define lconstant busy_cb(w, client, call);
    lvars w, client, call, new;
    if call == REFRESH then
        ;;; we are being called to refresh the busy cursor since its changed
        if DPY(client) == new_dpy then
            XptNewCursorPlaneCursor(w, new_cursor)->;
        endif;
    elseif call then
        ;;; install busy cursor - save old cursor
        XptBusyCursor(DPY(client)) -> new;
        XptNewCursorPlaneCursor(w, new) -> CURSOR(client);
    else
        ;;; restore old cursor
        XptNewCursorPlaneCursor(w, CURSOR(client))->;
    endif;
enddefine;

define global XptBusyCursorFeedback(widget);
    lvars widget;
    busy_widgets(XptCheckWidget(widget)) and true;
enddefine;

define updaterof XptBusyCursorFeedback(enabled, widget);
    lvars enabled, widget, pair;
    dlocal pop_asts_enabled = false;
    busy_widgets(XptCheckWidget(widget))->pair;
    if enabled and not(pair) then
        ;;; setup for new widget
        true -> XptCursorPlaneOn(widget);
        XptBusyCursor(fast_XtDisplay(widget))->;
        conspair(fast_XtDisplay(widget), false)->>busy_widgets(widget) -> pair;
        XptAddClientCallback(widget, BUSY_CB_NAME, busy_cb, pair);
        if client_busy then
            XptCallClientCallbacks(widget, BUSY_CB_NAME, true);
        endif;
    elseif not(enabled) and pair then
        ;;; disable for old widget
        false -> busy_widgets(widget);
        if client_busy then
            XptCallClientCallbacks(widget, BUSY_CB_NAME, false);
        endif;
        XptRemoveClientCallback(widget, BUSY_CB_NAME, busy_cb, pair);
        sys_grbg_list(pair);
    endif;
enddefine;

;;; "clientBusy"

define global active XptBusyCursorOn;
    client_busy;
enddefine;

define updaterof active XptBusyCursorOn(bool);
    lvars bool;

    define lconstant cursor_change(cursor, dpy) -> (cursor, dpy);
        lvars cursor, dpy;
        returnunless(client_busy);
        ;;; we need to update the cursor shown for this display:
        cursor -> new_cursor; dpy -> new_dpy;
        dlocal pop_asts_enabled = false;
        XptCallAllClientCallbacks(BUSY_CB_NAME, REFRESH);
    enddefine;

    returnif(bool == client_busy);

    dlocal pop_asts_enabled = false;
    unless trap_assigned then
        ;;; trap any changes to the busy cursor
        if XptBusyCursorChangeTrap == identfn then
            cursor_change
        else
            cursor_change <> XptBusyCursorChangeTrap
        endif -> XptBusyCursorChangeTrap;
        true -> trap_assigned
    endunless;

    XptCallAllClientCallbacks(BUSY_CB_NAME, bool);
    bool -> client_busy;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 16 1993
        Replaced compile-time addition to updater of XptBusyCursor with
        use of new XptBusyCursorChangeTrap assigned by updater of
        XptBusyCursorOn.
 */
