/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/ui/lib/S-poplog_uiS-controltool_xol.p
 > Purpose:         Open Look Poplog Control Panel
 > Author:          Julian Clinton, May 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now Xol;

section $-poplog_ui;

include pop_uiP.ph
include xpt_coretypes.ph;
include XolConstants.ph;

uses
    xolFormWidget,
    xolControlAreaWidget,
    xolMenuButtonWidget,
    xolNonexclusivesWidget,
    xolOblongButtonWidget,
    xolRectButtonWidget,
    xolStaticTextWidget,
;

lconstant controltool_switch_vec    = INIT_controltool_switch_vec;
constant controltool_xol            = controltool_switch_vec;

    ;;; widget class for menu separator
xolStaticTextWidget -> wc_SEPARATOR;

define :macexpr p_CREATE_CONTROL_AREAS(parent,
            menubar_name, button_panel_name) -> (menubar, button_panel);
    lvars parent, menubar_name, menubar, button_panel_name,
            button_panel = false;

    lvars main_window = XtVaCreateWidget('_mainWindow', xolControlAreaWidget,
        parent, (#|
            XtN layoutType, OL_FIXEDCOLS,
            XtN hPad,       10,
            XtN vPad,       5,
            XtN vSpace,     15,
            ;;; the default value of XtNsameSize (OL_COLUMNS) is OK if
            ;;; the menu bar is narrower than the button panel: it will
            ;;; stretch to fit. If the menu bar grows wider, uncomment
            ;;; this next line
            /*
            XtN sameSize,   OL_NONE,
            */
            XtN center,     true,
        |#));
    lvars menubar = XtVaCreateManagedWidget(menubar_name, xolFormWidget,
        main_window, 0);

    if button_panel_name then
        lvars button_panel = XtVaCreateManagedWidget(button_panel_name,
            xolNonexclusivesWidget, main_window, (#|
                XtN layoutType, OL_FIXEDROWS,
            |#));
    endif;

    XtManageChild(main_window);

    ;;; Window has no sensible resize behaviour, so ask Olwm not to
    ;;; offer the option
    if XtIsSubclass(parent, xtTopLevelShellWidget) then
        false -> XptVal parent(XtN resizeCorners:XptBoolean);
    endif;
enddefine;

define :macexpr p_ADD_MENU(parent, label, is_help_menu, popup_cb) -> menu;
    lvars parent, label, is_help_menu, popup_cb, menu;
    lvars button = XtVaCreateManagedWidget(label, xolMenuButtonWidget,
        parent, 0);
    lvars menu = XptVal button(XtN menuPane:XptWidget);
    if popup_cb then
        ;;; function to call immediately before popup: menu is the
        ;;; client data
        lvars shell = XptShellOfObject(menu);
        XtAddCallback(shell, XtN popupCallback, popup_cb, menu);
    endif;
    ;;; add constraints for parent form
    returnunless(XtIsSubclass(parent, xolFormWidget));
    l_typespec children :XptWidget[];
    lvars (children, num_children) = XptVal parent(XtN children:exptr,
        XtN numChildren:XptCardinal);
    true -> XptVal button(XtN xAddWidth:XptBoolean);
    if is_help_menu then
        ;;; attach to right-hand edge
        (true, true) -> XptVal button(XtN xAttachRight:XptBoolean,
            XtN xVaryOffset:XptBoolean);
    endif;
    lvars i;
    for i from num_children-1 by -1 to 1 do
        lvars child = exacc[fast] children[i];
        if XptVal child(XtN xAttachRight:XptBoolean) then
            button -> XptVal child(XtN xRefWidget:XptWidget);
        else
            (child, 8) -> XptVal button(XtN xRefWidget:XptWidget,
                XtN xOffset:int);
            quitloop;
        endif;
    endfor;
enddefine;

define :macexpr p_ADD_MENU_BUTTON(parent, label, default, popup, cb, client)
        -> button;
    lvars parent, label, default, popup, cb, client, button;
    lvars button = XtVaCreateManagedWidget(label, xolOblongButtonWidget,
        parent, (#|
            XtN default, default,
            if popup then
                XtN labelType, OL_POPUP,
            endif;
        |#));
    XtAddCallback(button, XtN select, cb, client);
enddefine;

define :macexpr p_ADD_SUBSYSTEM_BUTTON(parent, ss, default, cb) -> button;
    lvars parent, ss, default, cb, button;
    lvars name = gen_ss_label(ss,2);
    lvars button = XtVaCreateManagedWidget(name, xolRectButtonWidget,
        parent, (#|
            XtN set,        false,
            XtN default,    default,
        |#));
    XtAddCallback(button, XtN select, cb, ss);
    XtAddCallback(button, XtN unselect, cb, ss);
enddefine;

endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jun 21 1995
        Changed layout of main control panel to reflect loss of one button
        off the menu bar, making it narrower than the button panel
--- Robert John Duncan, May  4 1995
        Changes for new window layout, with menu bar, etc.
--- John Gibson, Jun 28 1993
        Changes for POPC
--- Adrian Howard, Jun  9 1993
        "New file" option now always gives you a new file
--- John Gibson, Apr  9 1993
        Uses Xol/xol*Widget instead of XptW*idgetSet
--- John Gibson, Apr  2 1993
        Moved endexload_batch to after 'uses xved'
--- John Gibson, Jan 18 1993
        Changed to allow for possibility of no top-level compiler (e.g.
        as in XVed saved image)
--- Adrian Howard, Nov 10 1992
        Made sure XVed is setup before attempting to get icon_window window
--- Adrian Howard, Sep 17 1992
        New file option now places ved command on status line
--- John Gibson, Sep 10 1992
        Changed to use XptVal
--- Adrian Howard, Aug 20 1992
        Set -pop_pr_quotes- to -false- in exit callback to stop quotes being
        placed around the newline.
--- Integral Solutions Ltd, Aug 10 1992 (Julian Clinton)
        Changed panel title.
--- John Gibson, Jul 31 1992
        Replaced current_subsystem_name with c*urrent_ss_name
--- Integral Solutions Ltd, Jun  2 1992 (Julian Clinton)
        Exit calls in -exit_cb- now defer applied (fixes isl-fr.4450 -
        VMS X Poplog apparently hanging on restart).
--- John Gibson, Mar 26 1992
        xved_get_icon_filename no longer in section xved
--- Simon Nichols, Dec  3 1991
        Fixed -checkload_demotool- not to call the demotool unless it
        really is defined.
        Changed Poplog window title.
--- Robert John Duncan, Dec  3 1991
        Stopped the subsystem callback from invoking immediate mode
--- Simon Nichols, Nov 27 1991
        Fixed to prevent the UI loading the demo system at compile time.
--- Adrian Howard, Oct 31 1991 : Changed to use -XptArgPtr-
Julian Clinton, 21/10/91
    Added hooks for the Poplog demonstration tool.
    Modified and re-enabled control panel destroy callback.
    Renamed -pop_control_panel- to -pop_ui_control_panel-.
Julian Clinton, 14/10/91
    Modified so busy cursor appears when invoking tools.
--- Jonathan Meyer, Sep 26 1991
        Added Xpt*DeferApply to pop_ui_propertytool again.
--- Jonathan Meyer, Sep 16 1991 Made icon window sensitive to
        prefered icon size.
--- Integral Solutions Ltd, Sep 12 1991 (Julian Clinton)
    Changed call of -pop_ui_logo-.
    Removed true assignment to -pop_record_writeable-.
--- Adrian Howard, Sep  2 1991 : "uses" typo fixed.
--- Jonathan Meyer, Sep  2 1991
        Reinstated icon windows. Added call to pop_ui_logo,
        Added gc/busy cursor stuff. Made it use an Application shell rather
        than a (buggy) BaseWindow shell.
--- Jonathan Meyer, Aug 29 1991 XpolDefaultSetup -> XptDefaultSetup
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Includes vedfile_struct.ph.
    Removed "constant procedure" from proc. definitions.
    Subsystem button labels now created using gen_ss_label.
    Assigning true to subsystem_button_set will also enable the subsystem
    button.
    Immediate mode file name is 'output.<subsystem-extension>'
Julian Clinton, 21/8/91
    Changed to use pop_ui_(edit/compile)tool procedures.
Julian Clinton, 15/8/91
    Selecting a new subsystem now starts up an immediate mode window.
Julian Clinton, 13/8/91
    Changed to use guiShells and pop_ui_app_shell.
Julian Clinton, 13/8/91
    Set control area hPad to 10.
Julian Clinton, 7/8/91
    Revised calls to xoledittool and xolcompiletool.
Julian Clinton, 2/8/91
    Changed 'M.L.' to 'ML'.
    Made 'Save files and exit' default if files need writing.
Julian Clinton, 17/7/91
    Added extra checking for "prolog" subsystem.
    Changed subsystem accessors so that "prolog" is converted to "top"
    rather than the other way round.
Julian Clinton, 16/7/91
    Made caption font variable.
Julian Clinton, 15/7/91
    Changed information message.
Julian Clinton, 12/7/91
    Moved XFlush call to be inside XptIsLiveType in updater of
        subsystem_button_set.
Julian Clinton, 10/7/91
    Moved startup_gui call and definition to poplog_ui.p
Julian Clinton, 8/7/91
    Removed increase to popmemlim (now in done in each tool).
Julian Clinton, 24/6/91
    Added boolean argument to xolinformation.
    Added x and y location args to be passed to the control panel.
Julian Clinton, 19/6/91
    Changed subsystem_changed_warn to update control panel.
Julian Clinton, 17/6/91
    Changed to use guiXlibs.
    Added X buffer flushing.
Julian Clinton, 13/6/91
    Added ref. widget arg to xol(edit,compile,help,library)tool.
Julian Clinton, 11/6/91
    Changed ss_button_selectable to subsystem_button_selectable.
    Changed ss_set_button to subsystem_button_set and added updater.
    Added CR/NL before switching subsystem in subsystem_cb.
 */
