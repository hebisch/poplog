/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-confirm_xm.p
 > Purpose:         Motif confirmation window
 > Author:          Julian Clinton, June 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now Xm;

section $-poplog_ui;

include pop_uiP.ph
include xt_constants.ph;
include XmConstants.ph;

uses
    xt_widget,
    xt_widgetinfo,
    xt_widgetclass,
    xt_callback,
    xt_event,
    xt_popup,
    xt_composite,
    xmDialogShellWidget,
    xmMessageBoxWidget,
;

lconstant confirm_switch_vec    = INIT_confirm_switch_vec;
constant confirm_xm             = confirm_switch_vec;

lvars
    confirm_shell = false,
    confirm_widget = false,
    button_list = false,
    confirm_in_use = false,
    action = false,
    action_num = 0,
;

/* -------------------------------------------------------------- *
    Confirm Tool
* -------------------------------------------------------------- */

define lconstant confirm_cb(widget, client, calldata);
    lvars widget, client, calldata;
    client -> action;
enddefine;

define lconstant popdown_cb(widget, client, calldata);
    lvars widget, client, calldata;
    false -> confirm_in_use;
enddefine;

define lconstant set_buttons(widget, options, n_options, default);
    lconstant defaults_vec = #_< {% XmDIALOG_OK_BUTTON,
                                    XmDIALOG_CANCEL_BUTTON,
                                    XmDIALOG_HELP_BUTTON %} >_#;
    lvars widget, options, default, n_options;

    subscrl(1, options) sys_>< nullstring,
    subscrl(2, options) sys_>< nullstring
        -> XptVal widget(XmN okLabelString:XmString,
                         XmN cancelLabelString:XmString);

    XtAddCallback(widget, XmN okCallback, confirm_cb, 1);
    XtAddCallback(widget, XmN cancelCallback, confirm_cb, 2);

    if n_options == 3 then
        XtAddCallback(widget, XtN helpCallback, confirm_cb, 3);
        subscrl(3, options) sys_>< nullstring
            -> XptVal widget(XmN helpLabelString:XmString);
    else
        XtUnmanageChild(XmMessageBoxGetChild(widget, XmDIALOG_HELP_BUTTON));
    endif;

    subscrv(default, defaults_vec) -> XptVal widget(XmN defaultButtonType);
enddefine;

define :macexpr p_CONFIRM(question, options, default, centre_text, ref_widget,
                                                    title) -> result;
    lvars   index, expln, question, options, default, centre_text, ref_widget,
            title, result, buttons, n_options;
    dlocal  XptWMProtocols = false;

    if confirm_in_use then
        mishap(0, 'Confirm dialog already in use');
    endif;

    listlength(options) -> n_options;
    unless n_options == 2 or n_options == 3 then
        mishap(options, 1,
               'Motif pop_ui_confirm can only take 2 or 3 options');
    endunless;

    unless XptIsLiveType(ref_widget, "Widget") then
        pop_ui_app_shell -> ref_widget;
    endunless;

    XtCreatePopupShell('confirm', xmDialogShellWidget, ref_widget,
            XptArgList([{allowShellResize ^false}
                        {deleteResponse ^XmDO_NOTHING}])) -> confirm_shell;

    XmStringCreate(title, XmSTRING_DEFAULT_CHARSET)
        -> title;

    XmCreateMessageBox(confirm_shell, 'confirm',
        XptArgList([{dialogType ^XmDIALOG_QUESTION}
                    {dialogTitle ^title}
                    {noResize ^true}
                   ])) -> confirm_widget;

    if centre_text then
        XmALIGNMENT_CENTER
    else
        XmALIGNMENT_BEGINNING
    endif -> XptVal confirm_widget(XmN alignment);

    XptCenterWidgetOn(confirm_shell, "screen");

    set_buttons(confirm_widget, options, n_options, default);

    question -> XptVal confirm_widget(XmN messageString:XmString);

    XtAddCallback(confirm_shell, XmN popdownCallback, popdown_cb, 1);

    lvars old_busy = XptBusyCursorOn;
    procedure;
        EXIT_ACTION((XtUnmanageChild(confirm_widget),
                                    old_busy -> XptBusyCursorOn,
                     false -> confirm_in_use));
        true -> confirm_in_use;
        true -> XptBusyCursorOn;
        XtManageChild(confirm_widget);
        XtPopup(confirm_shell, XtGrabNone);
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
--- John Gibson, Apr 16 1993
        Changed to use Xm/xm*Widget etc.
--- John Gibson, Sep 10 1992
        Changed to use XptVal
--- John Williams, Aug 14 1992
        Added first arg of 0 to first call of -mishap-
--- Integral Solutions Ltd, Jun  2 1992
    EXIT_ACTION now assigns false to confirm_in_use and
    removed false assignment to confirm_in_use from callback.
    Relocated check for the in_use flag.
Julian Clinton, 10/12/91
    Set XptWMProtocols -false- and modified deleteResponse.
Julian Clinton, 14/10/91
    Made name strings consistent with OPEN LOOK.
    Removed calls to -section_import-.
--- Integral Solutions Ltd, Sep 12 1991 (Julian Clinton)
    Centered shell widget on screen.
--- Jonathan Meyer, Sep 11 1991 Now uses XtAppProcessEvent again
--- Jonathan Meyer, Sep  2 1991 Changed to used syshibernate. Fixed dlocals.
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Removed popmemlim increment (now done in pop_ui_popcontroltool.p).
Julian Clinton, 13/8/91
    Changed to use guiShells and pop_ui_app_shell.
Julian Clinton, 12/7/91
    Re-wrote to use a question dialog. Unfortunately this means that the
    number of options is limited to either two or three.
Julian Clinton, 8/7/91
    Increased popmemlim.
Julian Clinton, 5/7/91
    Added optional title arg.
 */
