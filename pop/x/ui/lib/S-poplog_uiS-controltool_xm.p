/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/ui/lib/S-poplog_uiS-controltool_xm.p
 > Purpose:         Motif Poplog Control Panel
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
include XmMwmUtil.ph;

uses
    xt_composite,

    xmMainWindowWidget,
    xmRowColumnWidget,
    xmFormWidget,
    xmPushButtonWidget,
    xmToggleButtonWidget,
    xmCascadeButtonWidget,
    xmLabelWidget,
    xmSeparatorWidget,
;

lconstant controltool_switch_vec    = INIT_controltool_switch_vec;
constant controltool_xm             = controltool_switch_vec;

    ;;; widget class for menu separator
xmSeparatorGadget -> wc_SEPARATOR;

define :macexpr p_CREATE_CONTROL_AREAS(parent,
            menubar_name, button_panel_name) -> (menubar, button_panel);
    lvars parent, menubar_name, menubar, button_panel_name,
            button_panel = false;

    lvars main_window =
        XtVaCreateWidget('_mainWindow', xmMainWindowWidget, parent, (#|
        |#));
    lvars menubar =
        XmCreateMenuBar(main_window, menubar_name, ConsArgList(#|
        |#));
    XtManageChild(menubar);
    menubar -> XptVal main_window(XmN menuBar:XptWidget);

    if button_panel_name then
        lvars form =
            XtVaCreateWidget('_workArea', xmFormWidget, main_window, (#|
                XmN shadowThickness,    2,
                XmN marginHeight,       10,
                XmN marginWidth,        10,
            |#));
        lvars button_panel =
            XmCreateRadioBox(form, button_panel_name, ConsArgList(#|
                XmN orientation,        XmHORIZONTAL,
                XmN adjustMargin,       true,
                XmN isAligned,          true,
                XmN entryAlignment,     XmALIGNMENT_CENTER,
                XmN topAttachment,      XmATTACH_FORM,
                XmN leftAttachment,     XmATTACH_FORM,
            |#));
        XtManageChild(button_panel);
        ;;; this next widget is needed on some (older?) Motif systems to
        ;;; prevent the form's shadow border being occluded by the
        ;;; button panel: it seems as though the marginWidth & Height
        ;;; resources are ignored for edges without explicit form
        ;;; attachments, but we don't want to add those attachments to
        ;;; the button panel itself because then it would resize
        ;;; horribly with the form
        lvars pad =
            XtVaCreateManagedWidget('_pad', xmSeparatorGadget, form, (#|
                XmN separatorType,      XmNO_LINE,
                XmN topAttachment,      XmATTACH_WIDGET,
                XmN topWidget,          button_panel,
                XmN topOffset,          0,
                XmN bottomAttachment,   XmATTACH_FORM,
                XmN leftAttachment,     XmATTACH_WIDGET,
                XmN leftWidget,         button_panel,
                XmN leftOffset,         0,
                XmN rightAttachment,    XmATTACH_FORM,
            |#));
        XtManageChild(form);
    endif;

    XtManageChild(main_window);

    ;;; Window has no sensible resize behaviour, so ask Mwm not to offer
    ;;; that option
    if XtIsSubclass(parent, xtTopLevelShellWidget) then
        XtVaSetValues(parent, (#| XmN mwmFunctions,
            ;;; ALL + others means subtract others
            MWM_FUNC_ALL||MWM_FUNC_RESIZE||MWM_FUNC_MAXIMIZE,
        |#));
    endif;
enddefine;

define :macexpr p_ADD_MENU(parent, label, is_help_menu, popup_cb) -> menu;
    lvars parent, label, is_help_menu, popup_cb, menu;
    lvars menu = XmCreatePulldownMenu(parent, 'menu', ConsArgList(0));
    lvars button =
        XtVaCreateManagedWidget(label, xmCascadeButtonGadget, parent, (#|
            XmN subMenuId,  menu
        |#));
    if popup_cb then
        ;;; function to call immediately before popup: menu is the
        ;;; client data; we use cascadingCallback on the button rather
        ;;; than popupCallback on the menu shell because there's some
        ;;; doubt about how well that's supported by Motif
        XtAddCallback(button, XmN cascadingCallback, popup_cb, menu);
    endif;
    if is_help_menu then
        button -> XptVal parent(XmN menuHelpWidget:XptWidget);
    endif;
enddefine;

define :macexpr p_ADD_MENU_BUTTON(parent, label, default, popup, cb, client)
        -> button;
    lvars parent, label, default, popup, cb, client, button;
    lvars button =
        XtVaCreateManagedWidget(label, xmPushButtonGadget, parent, (#|
            if popup then
                XmN labelString, ->XpmCoerceCopiedString(label <> '...'),
            endif;
        |#));
    XtAddCallback(button, XmN activateCallback, cb, client);
enddefine;

define :macexpr p_ADD_SUBSYSTEM_BUTTON(parent, ss, default, cb) -> button;
    lvars parent, ss, default, cb, button;
    lvars name = gen_ss_label(ss);
    lvars button =
        XtVaCreateManagedWidget(name, xmToggleButtonGadget, parent, (#|
            XmN set,                false,
            XmN indicatorOn,        false,
            XmN marginHeight,       5,
            XmN marginWidth,        5,
            XmN shadowThickness,    3,
        |#));
    if default then
        button -> XptVal parent(XmN initialFocus:XptWidget);
    endif;
    XtAddCallback(button, XmN valueChangedCallback, cb, ss);
enddefine;

endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jun 22 1995
        Fixed to use XpmCoerceCopiedString
--- Robert John Duncan, May  4 1995
        Changes for new window layout, with menu bar, etc.
--- John Gibson, Jun 28 1993
        Changes for POPC
--- Adrian Howard, Jun  9 1993
        "New file" option now always gives you a new file
--- John Gibson, Apr 16 1993
        Uses Xm/xm*Widget etc
--- John Gibson, Apr  2 1993
        Moved endexload_batch to after 'uses xved'
--- John Gibson, Jan 18 1993
        Changed to allow for possibility of no top-level compiler (e.g.
        as in XVed saved image)
--- Simon Nichols, Nov 11 1992
        Moved the guts of -exit_cb- into a new procedure -do_exit_action-
        called via -Xpt*DeferApply- to fix problem with exit button on VMS.
        Fix from Julian Clinton.
--- Adrian Howard, Nov 10 1992
        Made sure XVed is setup before attempting to get icon_window window
--- Adrian Howard, Sep 17 1992
        New file option now places ved command on status line
--- John Gibson, Sep 10 1992
        Changed to use XptVal
--- Adrian Howard, Aug 20 1992
        Set -pop_pr_quotes- to -false- in exit callback so quotes are not
        displayed around the newline character
--- Integral Solutions Ltd, Aug 10 1992 (Julian Clinton)
        Changed panel title.
--- John Gibson, Jul 31 1992
        Replaced current_subsystem_name with cu*rrent_ss_name
--- Integral Solutions Ltd, Jun  2 1992 (Julian Clinton)
        Exit calls in -exit_cb- now defer applied (fixes isl-fr.4450 -
        VMS X Poplog hanging on restart).
--- John Gibson, Mar 26 1992
        xved_get_icon_filename no longer in section xved
Julian Clinton, 10/12/91
    Set XptWMProtocols -false- and modified deleteResponse.
--- Simon Nichols, Dec  3 1991
        Fixed -checkload_demotool- not to call the demotool unless it
        really is defined.
        Changed Poplog window title.
--- Robert John Duncan, Dec  3 1991
        Stopped the subsystem callback from invoking immediate mode
--- Simon Nichols, Nov 27 1991
        Fixed to prevent the UI loading the demo system at compile time.
Julian Clinton, 21/10/91
    Added hooks for the Poplog demonstration tool.
    Modified and re-enabled control panel destroy callback.
    Renamed -pop_control_panel- to -pop_ui_control_panel-.
Julian Clinton, 14/10/91
    Modified so busy cursor appears when invoking tools.
--- Jonathan Meyer, Sep 16 1991 Made icon window sensitive to
        prefered icon size.
--- Integral Solutions Ltd, Sep 12 1991 (Julian Clinton)
    Changed call of -pop_ui_logo-.
    Removed true assignment to -pop_record_writeable-.
--- Adrian Howard, Sep  3 1991 : -uses- typo fixed
--- Jonathan Meyer, Sep  2 1991
        Reinstated icon windows. Added call to pop_ui_logo,
        Added gc/busy cursor stuff.
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Includes vedfile_struct.ph.
    Removed "constant procedure" from proc. definitions.
    Subsystem button labels now created using gen_ss_label.
    Assigning true to subsystem_button_set will laso enable the subsystem
    button.
    Immediate mode file name is 'output.<subsystem-extension>'
Julian Clinton, 21/8/91
    Changed to use pop_ui_(edit/compile)tool procedures.
Julian Clinton, 15/8/91
    Selecting a new subsystem now starts up an immediate mode window.
Julian Clinton, 13/8/91
    Changed to use guiShells.
Julian Clinton, 8/8/91
    Revised calls to xmledittool and xmlcompiletool.
Julian Clinton, 2/8/91
    Changed 'M.L.' to 'ML'.
    Made 'Save files and exit' default if files need writing.
Julian Clinton, 17/7/91
    Added extra checking for "prolog" subsystem.
    Changed subsystem accessors so that "prolog" is converted to "top"
    rather than the other way round.
Julian Clinton, 15/7/91
    Changed information message.
Julian Clinton, 12/7/91
    Moved XFlush call to be inside XptIsLiveType in updater of
        subsystem_button_set.
Julian Clinton, 10/7/91
    Moved startup_gui call and definition to poplog_ui.p
Julian Clinton, 8/7/91
    Removed increase to popmemlim (now in done in each tool).
 */
