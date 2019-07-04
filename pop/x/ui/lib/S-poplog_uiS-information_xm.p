/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-information_xm.p
 > Purpose:         Motif information board
 > Author:          Julian Clinton, June 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now Xm;

section $-poplog_ui;

include pop_uiP.ph;
include xt_constants.ph;
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

lconstant information_switch_vec= INIT_information_switch_vec;
constant information_xm         = information_switch_vec;


/* ---------------------------------------------------------------
    Creation Routine
   --------------------------------------------------------------- */

define :macexpr p_INFORMATION(info, centre_text, ref_widget, title)
                                                            -> info_shell;
    lvars   info, centre_text, ref_widget, widget, title,
            uppercontrol_widget, text_widget, info_widget, info_shell;
    dlocal XptWMProtocols = false;

    unless isXptDescriptor(ref_widget) then
        pop_ui_app_shell -> ref_widget;
    endunless;

    XtCreatePopupShell('information', xmDialogShellWidget,
            ref_widget,
            XptArgList([
                        {allowShellResize ^false}
                        {deleteResponse ^XmUNMAP}
                ])) -> info_shell;

    XmStringCreate(title, XmSTRING_DEFAULT_CHARSET)
        -> title;

    XmCreateMessageBox(info_shell, 'information',
        XptArgList([{dialogType ^XmDIALOG_INFORMATION}
                    {dialogTitle ^title}
                    {noResize ^true}
                   ])) -> info_widget;

    if centre_text then
        XmALIGNMENT_CENTER
    else
        XmALIGNMENT_BEGINNING
    endif -> XptVal info_widget(XmN alignment);

    XmDIALOG_CANCEL_BUTTON -> XptVal info_widget(XmN defaultButtonType);

    'Dismiss', info -> XptVal info_widget(XmN cancelLabelString:XmString,
                                          XmN messageString:XmString);

    XtUnmanageChild(XmMessageBoxGetChild(info_widget, XmDIALOG_OK_BUTTON));

    XtUnmanageChild(XmMessageBoxGetChild(info_widget, XmDIALOG_HELP_BUTTON));

    XtManageChild(info_widget);
    XtPopup(info_shell, XtGrabNone);
enddefine;

endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 28 1993
        Changes for POPC
--- John Gibson, Apr 16 1993
        Uses Xm/xm*Widget etc
--- John Gibson, Sep 10 1992
        Changed to use XptVal
Julian Clinton, 10/12/91
    Set XptWMProtocols -false- and modified deleteResponse.
Julian Clinton, 14/10/91
    Made name strings consistent with OPEN LOOK.
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Removed popmemlim increment (now done in pop_ui_popcontroltool.p).
Julian Clinton, 15/8/91
    Changed button label to 'Dismiss'.
Julian Clinton, 13/8/91
    Changed to use guiShells and pop_ui_app_shell.
Julian Clinton, 8/7/91
    Increased popmemlim.
Julian Clinton, 5/7/91
    Added optional title arg.
 */
