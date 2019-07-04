/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-information_xol.p
 > Purpose:         Open Look information board
 > Author:          Julian Clinton, June 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now Xol;

section $-poplog_ui;

include pop_uiP.ph;
include xpt_xtypes.ph;
include XolConstants.ph;

uses
    xt_init,
    xt_widget,
    xt_popup,
    xt_callback,

    xolStaticTextWidget,
    xolPopupWindowShellWidget,
    xolOblongButtonWidget,

    $-poplog_ui$-guiUtils,
    $-poplog_ui$-guiShells,
;

lconstant information_switch_vec= INIT_information_switch_vec;
constant information_xol        = information_switch_vec;


/* ---------------------------------------------------------------
    Callback
   --------------------------------------------------------------- */

;;; Called when the 'Dismiss' button is pressed
define lconstant info_dismiss_cb(w,client,call);
    lvars w, client, call;
    XtDestroyWidget(client);
enddefine;


/* ---------------------------------------------------------------
    Creation Routine
   --------------------------------------------------------------- */

define :macexpr p_INFORMATION(info, centre_text, ref_widget, title) -> widget;
    lvars   info, centre_text, ref_widget, widget, title,
            uppercontrol_widget, text_widget, lowercontrol_widget,
            dismiss_widget;

    unless pop_ui_app_shell then
        XptDefaultSetup();
    endunless;

    if isXptDescriptor(ref_widget) then
        ref_widget
    else
        pop_ui_app_shell
    endif -> ref_widget;

    XtVaCreatePopupShell('information', xolPopupWindowShellWidget, ref_widget,
            XptVaArgList([  {allowShellResize ^false} {pushpin ^OL_IN}
                            {borderWidth 4}])) -> widget;

    title -> XptVal widget(XtN title:XptString);

    XptVal widget(XtN upperControlArea:XptWidget) -> uppercontrol_widget;

    XtVaCreateManagedWidget('text', xolStaticTextWidget,
        uppercontrol_widget,
        XptVaArgList([])) -> text_widget;

    if centre_text then
        OL_CENTER
    else
        OL_LEFT
    endif, info -> XptVal text_widget(XtN alignment, XtN string:XptString);

    XptVal widget(XtN lowerControlArea:XptWidget) -> lowercontrol_widget;

    true -> XptVal lowercontrol_widget(XtN center:XptBoolean);

    XtVaCreateManagedWidget('Dismiss', xolOblongButtonWidget,
        lowercontrol_widget,
        XptVaArgList([])) -> dismiss_widget;

    XtAddCallback(dismiss_widget, XtN select, info_dismiss_cb, widget);

    XtPopup(widget, 0);
enddefine;

endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 28 1993
        Changes for POPC
--- John Gibson, Apr  9 1993
        Uses Xol/xol*Widget instead of XptW*idgetSet
--- John Gibson, Sep 10 1992
        Changed to use XptVal
Julian Clinton, 14/10/91
    Made name strings consistent with Motif.
--- Jonathan Meyer, Aug 29 1991 XpolDefaultSetup -> XptDefaultSetup
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Removed popmemlim increment (now done in pop_ui_popcontroltool.p).
Julian Clinton, 13/8/91
    Changed to use guiShells and pop_ui_app_shell.
Julian Clinton, 13/8/91
    Set borderWidth to 4.
Julian Clinton, 2/8/91
    Added 'Dismiss' button.
Julian Clinton, 8/7/91
    Increased popmemlim.
Julian Clinton, 5/7/91
    Added optional title arg.
Julian Clinton, 24/6/91
    Added boolean argument to allow text to be centered.
 */
