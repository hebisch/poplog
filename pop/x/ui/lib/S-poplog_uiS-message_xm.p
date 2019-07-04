/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-message_xm.p
 > Purpose:         Motif message window
 > Author:          Julian Clinton, May 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now Xm;

section $-poplog_ui;

include pop_uiP.ph;
include xt_constants;
include XmConstants.ph;

uses
    xt_init,
    xt_widget,
    xt_widgetclass,
    xt_widgetinfo,
    xt_composite,
    xt_popup,
    xt_event,
    xt_callback,

    xmDialogShellWidget,
    xmMessageBoxWidget,
;

lconstant message_switch_vec= INIT_message_switch_vec;
constant message_xm         = message_switch_vec;


lvars
    action = false,
    message_widget = false,
    message_shell = false,
    message_in_use = false,
;

/* -------------------------------------------------------------- *
    Callbacks
 * -------------------------------------------------------------- */

define lconstant cancel_cb(w, client, calldata);
lvars w client calldata;
    true -> action;
enddefine;


/* -------------------------------------------------------------- *
    Message
 * -------------------------------------------------------------- */

define :macexpr p_MESSAGE(message, centre_text, ref_widget, title);
    lvars message centre_text ref_widget title;
    dlocal XptWMProtocols = false;

    if message_in_use then
        mishap(0, 'Message dialog already in use');
    endif;

    unless isXptDescriptor(ref_widget) then
        pop_ui_app_shell -> ref_widget;
    endunless;

    XtCreatePopupShell('message', xmDialogShellWidget, ref_widget,
            XptArgList([
                        {allowShellResize ^false}
                        {deleteResponse ^XmDO_NOTHING}
                ])) -> message_shell;

    XmStringCreate(title, XmSTRING_DEFAULT_CHARSET)
        -> title;

    XmCreateMessageBox(message_shell, 'message',
        XptArgList([{dialogType ^XmDIALOG_MESSAGE}
                    {noResize ^true}
                    {dialogTitle ^title}
                   ])) -> message_widget;

    if centre_text then
        XmALIGNMENT_CENTER
    else
        XmALIGNMENT_BEGINNING
    endif -> XptVal message_widget(XmN alignment);

    XptCenterWidgetOn(message_shell, "screen");

    XmDIALOG_OK_BUTTON, message
        -> XptVal message_widget(XmN defaultButtonType,
                                 XmN messageString:XmString);

    XtUnmanageChild(XmMessageBoxGetChild(message_widget,
                                         XmDIALOG_CANCEL_BUTTON));

    XtUnmanageChild(XmMessageBoxGetChild(message_widget,
                                         XmDIALOG_HELP_BUTTON));

    XtAddCallback(message_widget, XmN okCallback, cancel_cb, 1);

    lvars old_busy = XptBusyCursorOn;
    procedure;
        EXIT_ACTION((XtUnmanageChild(message_widget),
                            old_busy -> XptBusyCursorOn,
                        false -> message_in_use));
        true -> message_in_use;
        true -> XptBusyCursorOn;
        XtManageChild(message_widget);
        XtPopup(message_shell, XtGrabNone);
        false -> action;
        until action do
            XtAppProcessEvent(XptDefaultAppContext, XtIMAll);
        enduntil;
    endprocedure();
    false -> action;
enddefine;

endsection;  /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 28 1993
        Changes for POPC
--- John Gibson, Apr 16 1993
        Uses Xm/xm*Widget etc
--- John Gibson, Sep 10 1992
        Changed to use XptVal
--- John Williams, Aug 14 1992
        Added first arg of 0 to first call of -mishap-
--- Integral Solutions Ltd, Jun  2 1992 (Julian Clinton)
    Now sets the busy cursor on and mishaps if in a nested call.
    EXIT_ACTION now assigns false to message_in_use and
    removed false assignment to message_in_use from callback.
    Relocated check for the in_use flag.
Julian Clinton, 10/12/91
    Set XptWMProtocols -false- and modified deleteResponse.
Julian Clinton, 14/10/91
    Made name strings consistent with OPEN LOOK.
Julian Clinton, 11/10/91
    Fixed cancel_cb to prevent '(LIVE) Widget NEEDED' mishaps.
--- Integral Solutions Ltd, Sep 12 1991 (Julian Clinton)
    Centered shell widget on screen.
--- Jonathan Meyer, Sep 11 1991 Now uses XtAppProcessEvent again
--- Jonathan Meyer, Sep  2 1991 Changed to use syshibernate
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Removed popmemlim increment (now done in pop_ui_popcontroltool.p).
Julian Clinton, 13/8/91
    Changed to use guiShells and pop_ui_app_shell.
Julian Clinton, 8/7/91
    Increased popmemlim.
Julian Clinton, 5/7/91
    Added optional title arg.
 */
