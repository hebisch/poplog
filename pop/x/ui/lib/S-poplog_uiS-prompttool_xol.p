/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/ui/lib/S-poplog_uiS-prompttool_xol.p
 > Purpose:         OpenLook prompt tool
 > Author:          John Gibson, Jun 20 1993 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

uses-now Xol;

section $-poplog_ui;

include pop_uiP.ph;
include xt_constants;
include xpt_xtypes;
include xpt_xscreen.ph;
include XolConstants.ph;

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
    xolNoticeShellWidget,
    xolOblongButtonGadget,
;

lconstant prompttool_switch_vec= INIT_prompttool_switch_vec;
constant prompttool_xol         = prompttool_switch_vec;

/*
The Xol Notice widget is very primitive - it doesn't recognize the return
key as an action to fire the default button. It also doesn't implement
OpenLook Popup mouse warping to warp the mouse onto the button. We implement
these here, but the code will propably become redundant when Sun improve
their widget.
*/


/* Xlib procedures to manage windows */
XptLoadProcedures pop_ui_prompttool
lvars
    XRaiseWindow    ;;; used to bring property sheet to the front.
    XWarpPointer    ;;; Prompts warp the pointer to the default button
    XQueryPointer   ;;; and restore it when its dismissed
    XLookupKeysym
;

lvars
    prompt_popup = false,
    prompt_result, prompt_message, prompt_buttons, prompt_default,
    prompt_mnemonics = [],
    save_x, save_y, save_screen = false,
;

l_typespec save_screen :XScreen;

                                                /* OLIT */
define lconstant Center_mouse_on(gadget);
    lvars   gadget, widget = gadget.XtParent, win = XtWindow(widget), dpy,
            status, x, y, width, height;

    l_typespec raw_XQueryPointer (9):XptLongBoolean;
    lconstant
        mouse_x = EXPTRINITSTR(:int),
        mouse_y = EXPTRINITSTR(:int),
        dummy   = EXPTRINITSTR(:XptXID),
    ;

    exacc raw_XQueryPointer(XtDisplay(widget), XtWindow(widget), dummy,
            dummy, mouse_x, mouse_y, dummy,dummy,dummy) -> status;
    if status then
        XptWidgetCoords(gadget) -> (x, y, width, height);
        exacc :int mouse_x -> save_x;  exacc :int mouse_y -> save_y;
        XtScreen(widget) -> save_screen;
        exacc save_screen.display -> dpy;
        exacc (9) raw_XWarpPointer(dpy, 0, win, 0,0,0,0,
                                    x+(width div 2), y+(height div 2))
    else
        false -> save_screen;
    endif;
enddefine;

                                                /* OLIT */
define lconstant Restore_mouse();
    lvars win dpy;
    if save_screen then
        exacc save_screen.root -> win;
        exacc save_screen.display -> dpy;
        exacc (9) raw_XWarpPointer(dpy, 0,win, 0,0,0,0, save_x, save_y);
    endif;
    false -> save_screen;
enddefine;

;;; Prompt callbacks

;;; Called when the window first gets mapped
                                                /* OLIT */
define lconstant Prompt_map_action(w, event, params, num_params);
    lvars w, event, params, num_params;
    true -> prompt_result;
enddefine;

;;; Called when the return key is pressed
                                                /* OLIT */
define lconstant Prompt_return_action(w, event, params, num_params);
    lvars w, event, params, num_params, key;
    if num_params == 0 then
        exacc (2):XptKeySym raw_XLookupKeysym(event, 0) -> key;
        if lmember(key, prompt_mnemonics) ->> key then
            hd(tl(key)) -> prompt_result;
        endif;
    else
        prompt_default -> prompt_result; ;;; 1st button is always default
    endif;
enddefine;

;;; Called when a button is pressed
                                                /* OLIT */
define lconstant Prompt_button_cb(w, client, call);
    lvars w, client, call;
    client -> prompt_result;
enddefine;

                                                /* OLIT */
define lconstant Prompt_popdown_cb(w, client, call);
    lvars w, client, call;
    Restore_mouse();
enddefine;

lvars dummyshell;

                                                /* OLIT */
define lconstant Create_prompt;
    lvars w b i trans;
    dlocal pop_asts_enabled = false;
    XptDefaultSetup();
    XptAppAddActionList(XptDefaultAppContext, [
        ['PromptReturnKey' ^Prompt_return_action]
        ['PromptMapAction' ^Prompt_map_action]
    ]);
    XtVaAppCreateShell('poplog', 'Poplog', xtApplicationShellWidget,
                    XptDefaultDisplay, 0) -> dummyshell;
    XtVaCreatePopupShell('prompt', xolNoticeShellWidget, dummyshell,
                    #| XtN input, true |#) -> prompt_popup;
    XtAddCallback(prompt_popup, XtN popdownCallback, Prompt_popdown_cb,false);

    ;;; Popup gets both a map action and a return key action
    XtParseTranslationTable('<Map>: PromptMapAction() \
             <Key>Return: PromptReturnKey(ok) \
                <Key>: PromptReturnKey()') -> trans;
    XtOverrideTranslations(prompt_popup, trans);

    ;;; Children just get a return key action
    XtParseTranslationTable('<Key>Return: PromptReturnKey(ok) \
            <Key>: PromptReturnKey()') -> trans;

    XptVal[fast] prompt_popup(XtN textArea:XptWidget) -> prompt_message;
    XtOverrideTranslations(prompt_message, trans);
    XptVal[fast] prompt_popup(XtN controlArea:XptWidget) -> w;
    XtOverrideTranslations(w, trans);
    {%fast_for i from 1 to 3 do
        XtVaCreateWidget('button' sys_>< i, xolOblongButtonGadget, w, 0) -> b;
        XtAddCallback(b, XtN select, Prompt_button_cb, i);
        b;
    endfast_for%} -> prompt_buttons;
    false -> pop_ui_promptsource;
enddefine;

define :macexpr p_PROMPTTOOL(title, type, message, aligned, labels, def);
    lvars title, i, widget, type, message, labels, aligned, label, def,
            num_labels = length(labels);

    lconstant prompt_names =
        [   message '\n'
            information 'Information:\n\n'
            warning 'Warning:\n\n'
            error 'Error:\n\n'
            working 'Please Wait:\n\n'
            question 'Please Confirm:\n\n'
        ];

    unless prompt_popup then Create_prompt() endunless;

    XtPopdown(prompt_popup);    ;;; ensure its down

    dlocal XptBusyCursorOn = true;
    dlocal 0 %, if dlocal_context < 3 then
                    XtPopdown(prompt_popup)
                endif %; ;;; ensure any exit forces it down

    if type then
        ((fast_lmember(type, prompt_names) ->> i) and i.tl.hd) or '\n' -> i;
        i sys_>< message -> message;
    endif;
    message <> '\n' -> message;
    ;;; set message
    message, aligned and OL_CENTER or OL_LEFT
        -> XptVal[fast] prompt_message(XtN string:XptString, XtN alignment);

    false -> prompt_default;
    ;;; set button labels. Only display buttons that are used.

    if prompt_buttons.length < num_labels then
        mishap(labels, prompt_buttons.length,2,'TOO MANY LABELS');
    elseif num_labels < 1 then
        mishap(labels,1, 'TOO FEW LABELS');
    endif;
    [] -> prompt_mnemonics;
    fast_for i in_vector prompt_buttons do XtUnmanageChild(i) endfast_for;
    [%fast_for i from 1 to num_labels do
        labels(i) -> label;
        prompt_buttons(i) -> widget;
        if label.islist or label.isvector then
            true;
            label(1) -> label; i -> prompt_default;
        elseif i == def then
            true;
            i -> prompt_default;
        else
            false
        endif, label sys_><''
         -> XptVal[fast] widget(XtN default:XptBoolean, XtN label:XptString);
        ;;; register for both upper and lower case keys
        XtManageChild(prompt_buttons(i));
        ;;; mnemonic information
        uppertolower(label(1)), i;
    endfast_for;%] -> prompt_mnemonics;

    unless prompt_default then
        1 -> prompt_default;
        true -> XptVal[fast] (prompt_buttons(1))(XtN default:XptBoolean);
    endunless;

    false -> prompt_result;
    XtPopup(prompt_popup, XtGrabExclusive);
    ;;; This following loop will finish when the Popup if complete - we don't
    ;;; want to warp the mouse before the window is visible, since the
    ;;; window manager might remanage and move the window.
    until prompt_result do
        fast_XtAppProcessEvent(XptDefaultAppContext, XtIMAll);
    enduntil;
    unless prompt_result.isinteger then
        if pop_ui_promptwarpmouse then
            Center_mouse_on(prompt_buttons(prompt_default));
        endif;
        false -> prompt_result;
        ;;; mini-event loop till the  clicks on a button
        until prompt_result do
            fast_XtAppProcessEvent(XptDefaultAppContext, XtIMAll);
        enduntil;
    endunless;
    sys_grbg_list(prompt_mnemonics);

    labels(prompt_result) -> label;
    if label.islist or label.isvector then label(1) else label endif;
    prompt_result;
enddefine;
                                                /* OLIT */
define :macexpr p_PROMPTSOURCE();
    unless prompt_popup then Create_prompt() endunless;
    XptCheckWidget(prompt_popup)->;
    XptVal[fast] prompt_popup(XtN emanateWidget:XptWidget);
enddefine;
;;;
define :macexpr updaterof p_PROMPTSOURCE(widget);
    lvars widget;
    unless prompt_popup then Create_prompt() endunless;
    XptCheckWidget(prompt_popup)->;
    widget -> XptVal[fast] prompt_popup(XtN emanateWidget:XptWidget);
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  1 1995
        Replaced use of int*vecs with EXPTRINITSTRs for correct types
--- John Gibson, Jun 28 1993
        Changes for POPC
 */
