/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-confirm_xol.p
 > Purpose:         Open Look confirmation window
 > Author:          Julian Clinton, May 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now Xol;

section $-poplog_ui;

include pop_uiP.ph
include xt_constants.ph;
include xpt_coretypes.ph;
include XolConstants.ph;

uses
    xt_widget,
    xt_callback,
    xt_event,
    xt_popup,
    xt_composite,
    xolNoticeShellWidget,
    xolOblongButtonWidget,
;

lconstant confirm_switch_vec    = INIT_confirm_switch_vec;
constant confirm_xol            = confirm_switch_vec;

lvars
    confirm_widget = false,
    confirm_text_widget = false,
    confirm_control_widget = false,
    button_list = false,
    confirm_in_use = false,
    action = false,
    action_num = 0,
    last_parent_widget = false, ;;; needed in case we need to re-parent
                                ;;; (i.e. re-create) the dialog
;

/* -------------------------------------------------------------- *
    Confirm Tool
* -------------------------------------------------------------- */

define lconstant confirm_cb(widget, client, calldata);
    lvars widget, client, calldata;
    client -> action;
enddefine;

define lconstant create_button(widget) -> button;
    lvars widget, button;

    XtCreateManagedWidget('Button', xolOblongButtonWidget,
                    confirm_control_widget,
                    XptArgList([{mappedWhenManaged ^false}])) -> button;
    action_num + 1 -> action_num;
    OL_CENTER -> XptVal button(XtN labelJustify);
    XtAddCallback(button, XtN select, confirm_cb, action_num);
enddefine;

define lconstant set_buttons(labels, default);
    lvars labels, button, label, index, default, len = length(labels);
    for index from 1 to len do
        subscrl(index, button_list) -> button;
        if isstring((subscrl(index, labels) ->> label)) then
            label
        else
            label sys_>< nullstring
        endif -> XptVal button(XtN label:XptString);
        if index == default then true else false endif
            -> XptVal button(XtN default:XptBoolean);
        XtManageChild(button);
        XtMapWidget(button);
    endfor;

    for index from len + 1 to action_num do
        subscrl(index, button_list) -> button;
        XtUnmanageChild(button);
    endfor;
enddefine;


define lconstant make_confirm(title, ref_widget);
    lvars title, ref_widget;

    unless pop_ui_app_shell then
        XptDefaultSetup();
    endunless;

    XtCreatePopupShell('confirm', xolNoticeShellWidget,
        (if XptIsLiveType(ref_widget, "Widget") then
            ref_widget
        else
            pop_ui_app_shell
        endif ->> last_parent_widget),
        XptArgList([{recomputeSize ^true}
                    {borderWidth 4}
                    ])) -> confirm_widget;

    ;;; set the text widget
    XptVal confirm_widget(XtN textArea:XptWidget) -> confirm_text_widget;

    true -> XptVal confirm_text_widget(XtN recomputeSize:XptBoolean);

    XptVal confirm_widget(XtN controlArea:XptWidget) -> confirm_control_widget;

    20, 20 -> XptVal confirm_control_widget(XtN hPad:short, XtN hSpace:short);

    ;;; reset button_list and action num when a new dialog is created
    pdtolist(create_button(%confirm_widget%)) -> button_list;
    0 -> action_num;

    XtRealizeWidget(confirm_widget);
enddefine;


define :macexpr p_CONFIRM(question, options, default, centre_text, ref_widget,
                                                title) -> result;
    lvars   index, question, options, default, centre_text, ref_widget,
            title, result, buttons, center_widget, old_busy = XptBusyCursorOn;

    if confirm_in_use then
        mishap(0, 'Confirm dialog already in use');
    endif;

    ;;; need to (re)build the dialog if:
    ;;;     - it hasn't been created before
    ;;;     - it has been created before but either we're trying
    ;;;       to re-parent it or the original parent has died
    if not(XptIsLiveType(confirm_widget, "Widget"))
      or ((ref_widget and (ref_widget /== last_parent_widget)))
      or not(XptIsLiveType(last_parent_widget, "Widget"))
      then
        make_confirm(title, ref_widget);
    endif;

    title -> XptVal confirm_widget(XtN title:XptString);

    set_buttons(options, default);

    if centre_text then
        OL_CENTER
    else
        OL_LEFT
    endif,
    question sys_>< '\n'
        -> XptVal confirm_text_widget(XtN alignment, XtN string:XptString);

    procedure;
        EXIT_ACTION((XtPopdown(confirm_widget), old_busy -> XptBusyCursorOn,
                    false -> confirm_in_use));
        true -> confirm_in_use;
        true -> XptBusyCursorOn;
        XptCenterWidgetOn(confirm_widget, "screen");
        XtPopup(confirm_widget, XtGrabExclusive);
        false -> action;
        until action do
            XtAppProcessEvent(XptDefaultAppContext, XtIMAll);
        enduntil;
    endprocedure();
    action -> result;
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
    EXIT_ACTION now assigns false to confirm_in_use and
    removed false assignment to confirm_in_use from callback.
    Relocated check for the in_use flag.
    Reset action_num when a new dialog window is created.
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
--- Jonathan Meyer, Sep  2 1991 Changed to use syshibernate. Fixed dlocals.
--- Jonathan Meyer, Aug 29 1991 Changed XpolDefaultSetup to XptDefaultSetup
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
Julian Clinton, 24/6/91
    Allowed numbers and words to be passed in the list as well as strings.
 */
