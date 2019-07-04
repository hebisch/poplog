/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-message_xol.p
 > Purpose:         Open Look message window
 > Author:          Julian Clinton, May 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now Xol;

section $-poplog_ui;

include pop_uiP.ph;
include xt_constants.ph;
include xpt_xtypes.ph;
include XolConstants.ph;

uses
    xt_init,
    xt_widget,
    xt_callback,
    xt_event,
    xt_popup,

    xolNoticeShellWidget,
    xolOblongButtonWidget,
;

lconstant message_switch_vec= INIT_message_switch_vec;
constant message_xol        = message_switch_vec;

lvars
    message_widget = false,
    message_text_widget = false,
    message_in_use = false,
    action = false,
    last_parent_widget = false, ;;; needed in case we need to re-parent
                                ;;; (i.e. re-create) the dialog
;

/* -------------------------------------------------------------- *
    Message
* -------------------------------------------------------------- */

define lconstant message_cb(widget, client, calldata);
lvars widget client calldata;
    true -> action;
enddefine;

define lconstant make_xolmessage(title, ref_widget);
    lvars message_text_area message_control_area message_button_widget
          message_control_widget message_txtcontrol_widget
          title ref_widget;

    unless pop_ui_app_shell then
        XptDefaultSetup();
    endunless;

    XtCreatePopupShell('message', xolNoticeShellWidget,
        (if XptIsLiveType(ref_widget, "Widget") then
            ref_widget
        else
            pop_ui_app_shell
        endif ->> last_parent_widget),
        XptArgList([{recomputeSize ^true}
                    {borderWidth 4}])) -> message_widget;

    XptVal message_widget(XtN textArea:XptWidget) -> message_text_widget;

    true -> XptVal message_text_widget(XtN wrap:XptBoolean);

    XptVal message_widget(XtN controlArea:XptWidget) -> message_control_widget;

    50 -> XptVal message_control_widget(XtN hSpace:short);

    XtCreateManagedWidget('Okay', xolOblongButtonWidget, message_control_widget,
        XptArgList([{default ^true}])) -> message_button_widget;

    XtAddCallback(message_button_widget, XtN select,
                  message_cb, undef);
    XtRealizeWidget(message_widget);
enddefine;


define :macexpr p_MESSAGE(message, centre_text, ref_widget, title);
    lvars message centre_text ref_widget center_widget title;

    if message_in_use then
        mishap(0, 'Message dialog already in use');
    endif;

    ;;; need to (re)build the dialog if:
    ;;;     - it hasn't been created before
    ;;;     - it has been created before but either we're trying
    ;;;       to re-parent it or the original parent has died
    if not(XptIsLiveType(message_widget, "Widget"))
      or ((ref_widget and (ref_widget /== last_parent_widget)))
      or not(XptIsLiveType(last_parent_widget, "Widget"))
      then
        make_xolmessage(title, ref_widget);
    endif;

    title -> XptVal message_widget(XtN title:XptString);

    if XptIsLiveType(ref_widget, "Widget") then
        ref_widget
    else
        pop_ui_app_shell
    endif ->> center_widget
          -> XptVal message_widget(XtN eminateWidget:XptWidget);

    if centre_text then
        OL_CENTER
    else
        OL_LEFT
    endif, message sys_>< '\n'
        -> XptVal message_text_widget(XtN alignment, XtN string:XptString);

    lvars old_busy = XptBusyCursorOn;
    procedure;
        EXIT_ACTION((XtPopdown(message_widget), old_busy -> XptBusyCursorOn,
                    false -> message_in_use));
        true -> message_in_use;
        true -> XptBusyCursorOn;
        XptCenterWidgetOn(message_widget, "screen");
        XtPopup(message_widget, XtGrabExclusive);
        false -> action;
        until action do
            XtAppProcessEvent(XptDefaultAppContext, XtIMAll);
        enduntil;
    endprocedure();
    false -> action;
enddefine;

endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 28 1993
        Changes for POPC
--- John Gibson, Apr  9 1993
        Uses Xol/xol*Widget instead of XptW*idgetSet
--- John Gibson, Sep 10 1992
        Changed to use XptVal
--- John Williams, Aug 14 1992
        Added first arg of 0 to first call of -mishap-
--- Integral Solutions Ltd, Jun  2 1992 (Julian Clinton)
    Made dialog re-create itself if either the parent has changed or
    has died.
    Now sets the busy cursor on and mishaps if in a nested call.
    EXIT_ACTION now assigns false to message_in_use and
    removed false assignment to message_in_use from callback.
    Relocated check for the in_use flag.
Julian Clinton, 14/10/91
    Made name strings consistent with Motif.
    Checks that the parent widget is a live widget type.
--- Integral Solutions Ltd, Sep 18 1991 (Julian Clinton)
    Modified centering behaviour to center on parent widget rather than
    screen (pop_ui_app_shell should be centered on screen anyway).
    Commented out assignment to eminateWidget resource.
--- Integral Solutions Ltd, Sep 12 1991 (Julian Clinton)
    Centered shell widget on screen.
--- Jonathan Meyer, Sep 11 1991 Now uses XtAppProcessEvent again
--- Jonathan Meyer, Aug 29 1991 XpolDefaultSetup -> XptDefaultSetup
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Removed popmemlim increment (now done in pop_ui_popcontroltool.p).
Julian Clinton, 13/8/91
    Changed to use guiShells and pop_ui_app_shell.
Julian Clinton, 13/8/91
    Set borderWidth to 4.
Julian Clinton, 9/7/91
    Changed to use XptIsLiveType.
Julian Clinton, 8/7/91
    Increased popmemlim.
Julian Clinton, 5/7/91
    Added optional title arg.
 */
