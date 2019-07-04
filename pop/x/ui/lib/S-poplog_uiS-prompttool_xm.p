/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/ui/lib/S-poplog_uiS-prompttool_xm.p
 > Purpose:         Motif prompt tool
 > Author:          John Gibson, Jun 20 1993 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

uses-now Xm;

section $-poplog_ui;

include pop_uiP.ph;
include xt_constants;
include xpt_xtypes;
include XmConstants.ph;

uses
    xt_display,
    xt_widget,
    xt_callback,
    xt_event,
    xt_popup,
    xt_widgetinfo,
    xt_composite,
    xt_trans,
    xt_action,
    fast_xt_util,
    fast_xt_grab,
    xmMessageBoxWidget,
;

lconstant prompttool_switch_vec= INIT_prompttool_switch_vec;
constant prompttool_xm          = prompttool_switch_vec;

lconstant prompt_dlgs = writeable {%
    XmCreateMessageDialog,
    XmCreateInformationDialog,
    XmCreateWarningDialog,
    XmCreateErrorDialog,
    XmCreateWorkingDialog,
    XmCreateQuestionDialog
%};


lvars prompt_result, toplevelshell, prompt_source, mapped;

                                                /* MOTIF */
define :macexpr p_PROMPTSOURCE();
    XptIsLiveType(prompt_source,"Widget") and prompt_source;
enddefine;
;;;
define :macexpr updaterof p_PROMPTSOURCE(widget);
    lvars widget;
    widget and XptCheckWidget(widget) -> prompt_source;
enddefine;

;;; Called when a button is pressed
                                                /* MOTIF */
define lconstant Prompt_button_cb(w, client, call);
    lvars w, client, call;
    client -> prompt_result;
enddefine;
                                                /* MOTIF */
define lconstant Prompt_map_cb(w, client, call);
    lvars w, client, call;
    true -> mapped;
enddefine;

                                                /* MOTIF */
define lconstant Create_prompt;
    lvars btn, w, b, i, trans, arglist, argcount;
    XptDefaultSetup();

    XtVaAppCreateShell('poplog', 'Poplog',
        xtApplicationShellWidget,
        XptDefaultDisplay,
        #|  XtN width,              1,
            XtN height,             1,
            XtN mappedWhenManaged,  false,
        |#) -> toplevelshell;

    XtRealizeWidget(toplevelshell);

    ConsArgList(#|
        XmN dialogStyle,        XmDIALOG_FULL_APPLICATION_MODAL,
        XmN autoUnmanage,       true,
        XmN defaultPosition,    false,
        XmN deleteResponse,     XmDO_NOTHING,
        XmN mappedWhenManaged,  false,
        XmN noResize,           true
    |#) -> (arglist, argcount);

    for i from 1 to datalength(prompt_dlgs) do
        prompt_dlgs(i)(toplevelshell, 'prompt', arglist, argcount)
            ->> w -> prompt_dlgs(i);
        XtAddCallback(w, XtN okCallback, Prompt_button_cb,1);
        XtAddCallback(w, XtN cancelCallback, Prompt_button_cb,2);
        XtAddCallback(w, XtN helpCallback, Prompt_button_cb,3);
        XtAddCallback(w, XtN mapCallback, Prompt_map_cb,false);
        XtRealizeWidget(w);
        XtSetMappedWhenManaged(w, true);
    endfor;

    false -> prompt_source;
enddefine;

                                                /* MOTIF */
define :macexpr p_PROMPTTOOL(title, type, message, aligned, labels, def);
    lvars aligned, title, i, widget, type, message, labels, label, def_btn,
            def, num_labels = length(labels);

    lconstant prompt_names = [
            message 1 information 2 warning 3 error 4
            working 5 question 6]
    ;

    define lconstant set_label(type, index, resname);
        lvars type, index, resname, btn;
        XmMessageBoxGetChild(widget, type) -> btn;
        if index > num_labels then
            XtUnmanageChild(btn);
        else
            XtManageChild(btn);
            labels(index) -> label;
            if label.islist or label.isvector then
                label(1) -> label;
                btn -> def_btn;
            elseif def == index then
                btn -> def_btn;
            endif;
            label sys_>< '' -> XptVal[fast] widget(%resname%:XmString);
        endif;
    enddefine;

    unless XptIsLiveType(fast_subscrv(1, prompt_dlgs), "Widget") then
        Create_prompt();
    endunless;

    ((fast_lmember(type, prompt_names) ->> i) and i.tl.hd) or 1 -> i;
    prompt_dlgs(i) -> widget;

    XtUnmanageChild(widget);    ;;; ensure its down
    dlocal 0 %, if dlocal_context < 3 then
                    XtUnmanageChild(widget)
            endif %; ;;; ensure any exit forces it down

    ;;; only three buttons allowed
    if num_labels > 3 then
        mishap(labels, 3, 2,'TOO MANY LABELS');
    elseif num_labels < 1 then
        mishap(labels,1, 'TOO FEW LABELS');
    endif;

    (title or nullstring,
        aligned and XmALIGNMENT_CENTER or XmALIGNMENT_BEGINNING,
        message sys_><'')
        -> XptVal[fast] widget(XtN dialogTitle:XmString,
                               XmN messageAlignment,
                               XtN messageString:XmString);

    XmMessageBoxGetChild(widget, XmDIALOG_OK_BUTTON) -> def_btn;
    ;;; set button labels
    set_label(XmDIALOG_OK_BUTTON, 1, XtN okLabelString);
    set_label(XmDIALOG_CANCEL_BUTTON, 2, XtN cancelLabelString);
    set_label(XmDIALOG_HELP_BUTTON, 3, XtN helpLabelString);

    false -> mapped;
    XptCenterWidgetOn(widget, pop_ui_promptsource or "screen");
    XptMaxWidgetVisibility(widget);
    XtManageChild(widget);
    XtPopup(XtParent(widget), XtGrabNone);
    false -> prompt_result;
    XmProcessTraversal(def_btn, XmTRAVERSE_CURRENT)->; ;;; arm default button
    ;;; mini-event loop till the  clicks on a button
    until prompt_result do
        fast_XtAppProcessEvent(XptDefaultAppContext, XtIMAll);
    enduntil;

    labels(prompt_result) -> label;
    if label.islist or label.isvector then label(1) else label endif;
    prompt_result;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Integral Solutions Ltd (Julian Clinton), Nov  5 1993
        Changed popup processing so extra wait loop removed (not needed).
--- John Gibson, Jun 28 1993
        Changes for POPC
 */
