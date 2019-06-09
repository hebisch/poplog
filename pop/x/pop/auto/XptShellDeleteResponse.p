/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptShellDeleteResponse.p
 > Purpose:         Changes handling of WM_DELETE_WINDOW messages
 > Author:          Jonathan Meyer, Nov 27 1991
 > Documentation:   REF *XptShellDeleteResponse
 > Related Files:
 */
compile_mode :pop11 +strict;
section;

exload_batch;

uses xpt_atomcache;
uses xt_event;
uses fast_xt_display;
uses fast_xt_widgetinfo;

include xpt_xevent.ph;
include xpt_constants.ph;
include xt_constants.ph;

endexload_batch;

lconstant actionsTable = newproperty([], 10, false, "tmparg");

define global XptShellDeleteResponse(widget);
    lvars widget, action;
    actionsTable(XptDescriptor(XptShellOfObject(widget), XDT_WIDGET))-> action;
    action and fast_back(action);
enddefine;

define updaterof XptShellDeleteResponse(response, widget);
    lvars response, widget, shell_widget;

    ;;; Client_message_cb:
    ;;;     event handler for ClientMessage events.
    ;;;     may  be  called  quite  frequently,  so  efficiency  counts
    define lconstant Client_message_cb(shell, client, event) -> carry_on;
        lvars shell, client, widget, event, action, carry_on = true, disp;
        l_typespec event :XClientMessageEvent;

        define :inline lconstant ATOM(name);
            XptInternAtom(disp, name, false)
        enddefine;

        if exacc [fast] event.type == ClientMessage and
          (fast_XtDisplay(shell)->disp,
          exacc [fast] event.message_type == ATOM("WM_PROTOCOLS")) and
          exacc :uint (exacc [@] event.data) == ATOM("WM_DELETE_WINDOW") then
            if actionsTable(XptDescriptor(shell, XDT_WIDGET)) ->> action then
                fast_destpair(action) -> (widget, action);
                if widget == undef then shell -> widget endif;
                if XptIsLiveType(widget, XDT_WIDGET) then
                    XptCallbackHandler(widget, action, "delete_window")
                endif;
                unless XptIsLiveType(widget, XDT_WIDGET) then
                    ;;; the delete_window action did me in
                    false -> actionsTable(shell);
                endunless;
                false -> carry_on;
            endif;
        endif;
    enddefine;

    XptDescriptor(XptShellOfObject(widget), XDT_WIDGET) -> shell_widget;

    unless response.isprocedure or response.isword or
            response.isident or not(response) then
        mishap(response,1,'WIDENTPROC or -false- NEEDED');
    endunless;

    unless fast_XtIsRealized(widget) then
        mishap(widget,1, 'REALIZED WIDGET NEEDED');
    endunless;

    if response and not(actionsTable(shell_widget)) then
        ;;; register event handler
        XtInsertEventHandler(shell_widget, NoEventMask, true,
                Client_message_cb, false, XtListHead);
    else
        ;;; unregister event handler
        XtRemoveEventHandler(shell_widget, NoEventMask, true,
                Client_message_cb, false);
    endif;
    ;;; we do this so we don't keep a handle on the shell widget.
    if widget == shell_widget then undef -> widget endif;
    response and conspair(widget, response) -> actionsTable(shell_widget);
enddefine;

endsection;
