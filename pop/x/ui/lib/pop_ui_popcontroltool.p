/* --- Copyright University of Sussex 1996.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/pop_ui_popcontroltool.p
 > Purpose:         Poplog Control Panel
 > Author:          Julian Clinton, July 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now popxlib;

section $-poplog_ui =>  pop_ui_control_panel,
                        pop_ui_popcontroltool,
                        current_subsystem_button,
                        subsystem_button_selectable;
exload_batch;

include pop_uiP.ph;
include xt_constants.ph;
include xpt_coretypes.ph;
include subsystem.ph;
include vedfile_struct.ph;

uses
    pop_ui_vedcomms,
    pop_ui_message,
    pop_ui_confirm,
    pop_ui_propertytool,

    xved (xvedscrollbar, xvedmenubar, xveddialogs),

    xt_widget,
    xt_callback,
    xt_popup,
    xt_event,
    xt_util,

    XptShellDeleteResponse,

    pop_ui_projecttool,

    $-poplog_ui$-guiUtils,
    $-poplog_ui$-guiFileutils,
    $-poplog_ui$-guiActions,
    $-poplog_ui$-guiXlibs,
    $-poplog_ui$-guiShells,
    $-poplog_ui$-guiSubsystem,
;

weak constant procedure $-pop_ui_demotool;

vars pop_ui_control_panel = false;

lvars
    controltool_switch_vec,
    controltool_shell       = false,
    controltool_icon_window = false,
    controltool_display     = false,
    ss_changed_warn_set     = false,
;


/* ---------------------------------------------------------------
    Subsystem info
   --------------------------------------------------------------- */

;;; subsystem_button_selectable takes a subsystem name and returns
;;; whether or not the subsystem button on the control panel is
;;; selectable
define subsystem_button_selectable(ss) -> activep;
    lvars ss, activep, widget;

    SWITCH_SS_CHECK(ss);

    if XptIsLiveType(controltool_shell, "Widget") and
      (panel_ss_table(ss) ->> widget) then
        XtIsSensitive(subscrv(SUBS_PANEL_BUTTON, widget));
    else
        undef
    endif -> activep;
enddefine;
;;;
;;; the updater can define whether the subsystem button is selectable
;;; or not in case a subsystem library is loaded at runtime
define updaterof subsystem_button_selectable(val, ss);
    lvars val, ss, table, widget;

    SWITCH_SS_CHECK(ss);

    if XptIsLiveType(controltool_shell, "Widget") and
      (panel_ss_table(ss) ->> table) then
        subscrv(SUBS_PANEL_BUTTON, table) -> widget;
        XtSetSensitive(widget, val);
        val -> subscrv(SUBS_SELECTABLE, table);
    endif;
enddefine;

define subsystem_button_set(ss) -> res;
    lvars ss, lookup, entry, res;

    SWITCH_SS_CHECK(ss);

    if XptIsLiveType(controltool_shell, "Widget") and
      (panel_ss_table(ss) ->> lookup) and
      (subscrv(SUBS_PANEL_BUTTON, lookup) ->> entry) then
        XptVal entry(XtN set:XptBoolean)    ;;; same Motif/OLIT
    else
        undef
    endif -> res;
enddefine;
;;;
define updaterof subsystem_button_set(val, ss);
    lvars val, ss, lookup;

    define lconstant reset_all_ss_buttons(label, entry);
    lvars label entry;

        if label == ss then
            val
        else
            not(val)
        endif -> XptVal (subscrv(SUBS_PANEL_BUTTON, entry))
                            (XtN set:XptBoolean);
    enddefine;

    SWITCH_SS_CHECK(ss);

    if XptIsLiveType(controltool_shell, "Widget") and
      (panel_ss_table(ss) ->> lookup) and
      subscrv(SUBS_PANEL_BUTTON, lookup) then

        if val and not(subsystem_button_selectable(ss)) then
            true -> subsystem_button_selectable(ss);
        endif;

        appproperty(panel_ss_table, reset_all_ss_buttons);
        XFlush(controltool_display);
    endif;
enddefine;

define active current_subsystem_button;
    sys_compiler_subsystem(`T`);
enddefine;
;;;
define updaterof active current_subsystem_button(ss);
    lvars ss;
    true -> subsystem_button_set(ss);
enddefine;

define lconstant checkload_subsystem(ss);
    lvars ss, info, file, title;

    SWITCH_SS_CHECK(ss);

    if (subsystem_datavec(ss) ->> info) then
        subscrv(SUBSDV_TITLE, info) -> title;
        subscrv(SUBSDV_LOADER, info) -> file;
        if file and isfile(file) then
            pop_ui_message('Sorry, ' sys_>< title sys_>< ' not loaded in this Poplog image.',
                       true, controltool_shell);
        else
            pop_ui_message('Sorry, ' sys_>< title sys_>< ' not installed.',
                       true, controltool_shell);
        endif;
    endif;
enddefine;


/* ---------------------------------------------------------------
    Callbacks
   --------------------------------------------------------------- */

    ;;; test whether there are Ved files to be saved
define lconstant files_to_save();
    lvars file;
    ved_save_file_globals();
    fast_for file in vedbufferlist do
        returnif(file(VF_CHANGED) and (file(VF_WRITEABLE) or vedwriteallfiles))
            (true);
    endfor;
    false;
enddefine;

    ;;; response to WM_DELETE_WINDOW on the control panel -- offer exit
    ;;; from Poplog
define lconstant delete_response(shell);
    lvars shell;
    dlocal pop_pr_quotes = false;
    lvars needs_write = files_to_save();
    lvars ans = pop_ui_confirm(
        'Please confirm exit from Poplog',
        [%  'Exit',
            if needs_write then 'Save Files and Exit' endif,
            'Cancel'
        %], if needs_write then 2 else 1 endif,
        true, shell);
    lvars exit =
        if ans == 1 then
            ved_rqq
        elseif ans == 2 then
            needs_write and sysexit
        else
            false
        endif;
    if exit then
        cucharout(`\n`);
        pop_ui_kill_project();
        exit();
    endif;
enddefine;

    ;;; callback for destroying the control panel
define lconstant destroy_cb(widget, clientdata, calldata);
    lvars widget, clientdata, calldata;

    ;;; reset some local and global vars if the window has been
    ;;; destroyed
    false ->> controltool_shell -> pop_ui_control_panel;

    ;;; also reset the pop_ui_setup_done flag (if defined) so that
    ;;; a call to pop_ui_setup will re-create the control panel
    if identprops("pop_ui_setup_done") /== "undef" then
        false -> valof("pop_ui_setup_done");
    endif;
enddefine;

    ;;; callback for pressing a subsystem button
define lconstant subsystem_cb(widget, clientdata, calldata);
    lvars widget, clientdata, calldata, curr;

    ;;; check to see if the subsystem is aleady loaded
    if is_subsystem_loaded(clientdata) then
        returnif(vedinvedprocess or not(sys_compiler_subsystem(`t`) ->> curr));
        true -> subsystem_button_set(clientdata);
        returnif(curr == clientdata);
        external_defer_apply(
            procedure(clientdata);
                lvars clientdata;
                cucharout(`\n`);
                clientdata -> sys_compiler_subsystem(`t`);
            endprocedure, clientdata, 1)
    else
        ;;; tell the user they will have to start a new Poplog or
        ;;; install the requested subsystem
        checkload_subsystem(clientdata);
    endif;
enddefine;

    ;;; default callback for menus: clientdata is a procedure taking no
    ;;; arguments which is called through external_defer_apply
define lconstant menu_cb(w, clientdata, calldata);
    lvars w, clientdata, calldata;
    external_defer_apply(clientdata);
enddefine;


;;; -- The File Menu ------------------------------------------------------

define lconstant file_open =
    call_ved(%'openfile'%);
enddefine;

define lconstant file_compile =
    call_ved(%'compilefile'%);
enddefine;

define lconstant file_interaction =
    call_ved(%'im'%);
enddefine;

define lconstant file_save_all =
    call_ved(%'w'%);
enddefine;

define lconstant file_exit();
    ;;; same as choosing Close from the WM menu
    delete_response(controltool_shell);
enddefine;

    ;;; callback for File menu popup: disables the SaveAll button if
    ;;; there's nothing to be saved
define lconstant file_popup_cb(widget, clientdata, calldata);
    lvars widget, clientdata, calldata;
    ;;; clientdata is the File menu
    lvars menu = exacc:XptWidget clientdata;
    lvars button = XtNameToWidget(menu, 'Save All');
    if button then
        XtSetSensitive(button, files_to_save());
    endif;
enddefine;


;;; -- The Project Menu ---------------------------------------------------

define lconstant project_new();
    dlocal XptBusyCursorOn = true;
    pop_ui_new_project();
enddefine;

define lconstant project_open();
    dlocal XptBusyCursorOn = true;
    pop_ui_open_project();
enddefine;

define lconstant project_save();
    dlocal XptBusyCursorOn = true;
    pop_ui_save_project();
enddefine;

define lconstant project_close();
    dlocal XptBusyCursorOn = true;
    pop_ui_close_project();
enddefine;

    ;;; callback for Project menu popup: disables save and close
    ;;; unless there is a project open
define lconstant project_popup_cb(widget, clientdata, calldata);
    lvars widget, clientdata, calldata;
    ;;; clientdata is the Project menu
    lvars menu = exacc:XptWidget clientdata;
    lvars (buttons, num_buttons) = XptVal menu(XtN children:exptr,
        XtN numChildren:XptCardinal);
    lvars i;
    for i from 3 to num_buttons do
        lvars button = exacc[fast]:XptWidget[] buttons[i];
        XtSetSensitive(button, pop_ui_current_project and true);
    endfor;
enddefine;


;;; -- The Tools Menu -----------------------------------------------------

define lconstant tool_libraries =
    call_ved(%'librarytool'%);
enddefine;

define lconstant tool_help =
    call_ved(%'helptool'%);
enddefine;

lconstant no_demos_msg =
'The Poplog Demonstration System has not been installed.\
\
Please consult the Poplog Installation Guide for information\
about installing optional parts of Poplog.\n';

define lconstant tool_demos();
    dlocal XptBusyCursorOn = true;
    ;;; if the demotool has not been loaded already ...
    unless testdef pop_ui_demotool then
        ;;; ... try to load it. If successful, invoke it, otherwise
        ;;; print out a message to that effect.
        subsystem_libcompile('pop_ui_demotool.p', popuseslist)-> ;
    endunless;
    if testdef pop_ui_demotool and isprocedure(weakref pop_ui_demotool) then
        weakref pop_ui_demotool();
    else
        pop_ui_message(no_demos_msg, false, controltool_shell);
    endif;
enddefine;


;;; -- The Options Menu ---------------------------------------------------

define lconstant options_save();
    dlocal XptBusyCursorOn = true;
    pop_ui_save_properties();
enddefine;

define lconstant options_display(title);
    dlocal XptBusyCursorOn = true;
    unless proptool_is_created(title) then
        ;;; first time -- set optimum position for a newly-created popup:
        ;;; reference point is the first button in the subsystems panel
        lvars w = XtNameToWidget(controltool_shell, '*subsystemButtons');
        if w and (w(1) ->> w) then
            l_typespec posn { x:XptPosition, y:XptPosition };
            lvars posn = EXPTRINITSTR(:posn);
            fast_XtTranslateCoords(w, 0,0, exacc[@]posn.x, exacc[@]posn.y);
            proptool_create(title, controltool_shell) -> w;
            (exacc posn.x, exacc posn.y, false, false) -> XptWidgetCoords(w);
            XptMaxWidgetVisibility(w);
        endif;
    endunless;
    pop_ui_show_property(title, controltool_shell);
enddefine;

    ;;; special callback for Options menu: clientdata may be the title
    ;;; of an options page to go to, or else a procedure to call
define lconstant option_cb(widget, clientdata, calldata);
    lvars widget, clientdata, calldata;
    external_defer_apply(
        if isstring(clientdata) then
            ;;; title of options page to go to
            options_display, clientdata, 1
        else
            clientdata
        endif);
enddefine;

    ;;; callback for Options menu popup: sets the menu buttons to
    ;;; reflect the currently available choice of options pages
define lconstant option_popup_cb(widget, clientdata, calldata);
    lvars widget, clientdata, calldata;
    ;;; clientdata is the option menu itself; other args are unknown
    lvars menu = exacc:XptWidget clientdata;
    ;;; set sensitivity of the first (Save) button depending on whether
    ;;; a save is needed
    lvars (buttons, num_buttons) = XptVal menu(XtN children:exptr,
        XtN numChildren:XptCardinal);
    if num_buttons > 0 then
        lvars button = exacc[fast]:XptWidget[] buttons[1];
        XtSetSensitive(button, proptool_needs_saving());
    endif;
    ;;; return if nothing's changed since last time
    define lconstant cache =
        newproperty([], 16, false, "tmparg");
    enddefine;
    lvars options = proptool_names();
    returnif(cache(menu) = options);
    ;;; unmanage existing buttons
    fast_XtUnmanageChildren(buttons, num_buttons);
    ;;; remanage the save button and separator
    XtManageChild(button);
    if num_buttons > 1 then
        XtManageChild(exacc[fast]:XptWidget[] buttons[2]);
    endif;
    ;;; add a button for each option sheet
    lvars option;
    for option in options do
        if cache(option) ->> button then
            ;;; reuse existing
            XtManageChild(button);
        else
            ;;; create new
            p_ADD_MENU_BUTTON(menu, option, false, true, option_cb, option)
                -> cache(option);
        endif;
    endfor;
    ;;; remember current state
    options -> cache(menu);
enddefine;


;;; -- The Help Menu ------------------------------------------------------

    ;;; special callback for the Help menu: clientdata is always a Ved
    ;;; command or procedure to run
define lconstant help_cb(widget, clientdata, calldata);
    lvars widget, clientdata, calldata;
    external_defer_apply(call_ved, clientdata, 1);
enddefine;


;;; -- The Menu Bar -------------------------------------------------------

lconstant

    SEPARATOR = {^false ^false ^false ^false},

    file_menu = {^menu_cb ^file_popup_cb {
        {'Open'         ^true   ^true   ^file_open}
        {'Compile'      ^false  ^true   ^file_compile}
        {'Library'      ^false  ^true   ^tool_libraries}
        {'Interaction'  ^false  ^false  ^file_interaction}
        {'Save All'     ^false  ^false  ^file_save_all}
        ^SEPARATOR
        {'Exit'         ^false  ^false  ^file_exit}
    }},

    project_menu = {^menu_cb ^project_popup_cb {
        {'New'          ^true   ^true   ^project_new}
        {'Open'         ^false  ^true   ^project_open}
        {'Save'         ^false  ^false  ^project_save}
        {'Close'        ^false  ^false  ^project_close}
    }},

    tool_menu = {^menu_cb ^false {
        {'Libraries'    ^true   ^true   ^tool_libraries}
        {'Help'         ^false  ^true   ^tool_help}
        {'Demos'        ^false  ^true   ^tool_demos}
    }},

    options_menu = {^option_cb ^option_popup_cb {
        {'Save Options' ^false  ^false  ^options_save}
        ^SEPARATOR
    }},

    help_editor_menu = {^help_cb ^false {
        {'Overview'     ^true   ^false  'help pop_ui_editor_overview'}
        {'XVed Reference'
                        ^false  ^false  'ref xved'}
        {'Command Reference'
                        ^false  ^false  'ref vedcomms'}
        {'Keyboard'     ^false  ^false  'hkeys'}
    }},
    help_languages_menu = {^help_cb ^false {
        {'Subsystems'   ^true   ^false  'pop11 help subsystems'}
        {'Pop-11'       ^false  ^false  'pop11 help pop11'}
        {'Prolog'       ^false  ^false  'pop11 help prolog'}
        {'Common Lisp'  ^false  ^false  'pop11 help clisp'}
        {'Standard ML'  ^false  ^false  'pop11 help pml'}
    }},
    help_menu = {^help_cb ^false {
        {'Overview'     ^true   ^false  'help pop_ui_overview'}
        {'Project Tool' ^false  ^false  'help pop_ui_projecttool'}
        {'Search'       ^false  ^true   'helptool'}
        ^SEPARATOR
        {'Editor'       ^false  ^false  ^help_editor_menu}
        {'Languages'    ^false  ^false  ^help_languages_menu}
        ^SEPARATOR
        {'About Poplog' ^false  ^true   'aboutpoplog'}
        {'News'         ^false  ^false  'pop11 help news'}
        {'Customer Support'
                        ^false  ^false  'pop11 help poplog'}
        {'User Group'   ^false  ^false  'pop11 help plug'}
        {'Licence'      ^false  ^false  'pop11 help poplog_licence'}
    }},

    menu_bar_menus = {
        {'File'         ^false  ^file_menu}
        {'Project'      ^false  ^project_menu}
;;;     {'Tools'        ^false  ^tool_menu}
        {'Options'      ^false  ^options_menu}
        {'Help'         ^true   ^help_menu}
    },
;

define lconstant add_menu(parent, name, is_help, item);
    lvars parent, name, is_help, item;
    lvars (action_cb, popup_cb, items) = explode(item);
    lvars menu = p_ADD_MENU(parent, name, is_help, popup_cb);
    fast_for item in_vector items do
        lvars (name, default, popup, item) = explode(item);
        if not(name) then
            ;;; separator
            XtVaCreateManagedWidget('_separator', wc_SEPARATOR, menu, 0) -> ;
        elseif isvector(item) then
            ;;; pull-right menu
            add_menu(menu, name, false, item);
        else
            p_ADD_MENU_BUTTON(menu, name, default, popup, action_cb, item) -> ;
        endif;
    endfor;
enddefine;


/* ---------------------------------------------------------------
    Creation Routines
   --------------------------------------------------------------- */


;;; pop_ui_popcontroltool takes two arguments (the x and y location
;;; of the controltool)
define pop_ui_popcontroltool(x, y);
    lvars x, y;

    unless XptDefaultDisplay then XptDefaultSetup() endunless;

    if XptIsLiveType(controltool_shell, "Widget") then
        XtDestroyWidget(controltool_shell);
        false -> controltool_shell;
        sys_process_destroy_action(dup(controltool_icon_window))();
        false -> controltool_icon_window
    endif;

    "x" -> vedusewindows;
    vedsetup();

    /* JM 8/7/91 - temporary measure to get icon_window window */
    lconstant bitmap_base_name = '$usepop/pop/x/ui/bitmaps/poplog';
    xved_create_icon_window(
            xved_get_icon_filename(pop_ui_app_shell, bitmap_base_name),
            false) -> controltool_icon_window;

    lvars title = 'Poplog ' sys_>< (pop_internal_version / 10000.0);
    XtVaCreatePopupShell('controlTool', xtTopLevelShellWidget,
        pop_ui_app_shell, (#|
            XtN iconWindow,         controltool_icon_window,
            XtN iconName,           'Poplog',
            XtN title,              title,
        |#)) -> controltool_shell;

    (isinteger(x) and x, isinteger(y) and y, false, false)
                            -> XptWMShellCoords(controltool_shell);

    XtAddCallback(controltool_shell, XtN destroyCallback, destroy_cb, 1);

    lvars default_ss = sys_compiler_subsystem(`T`) or
        iscaller(syssetup) and caller_valof(ident subsystem, setpop);

    lvars (menu_bar, ss_buttons) = p_CREATE_CONTROL_AREAS(controltool_shell,
            'menuBar', default_ss and 'subsystemButtons');

    lvars item;
    fast_for item in_vector menu_bar_menus do
        add_menu(menu_bar, explode(item));
    endfor;

    if default_ss then

        lvars ss, table, default;
        for ss in panel_ss_ordering do
            initv(2) ->> table -> panel_ss_table(ss);
            ss == default_ss or (ss == "top" and default_ss == "prolog")
                                    -> default;
            p_ADD_SUBSYSTEM_BUTTON(ss_buttons, ss, default, subsystem_cb)
                -> subscrv(SUBS_PANEL_BUTTON, table);
            false -> subscrv(SUBS_SELECTABLE, table);
            is_subsystem_loaded(ss) -> subsystem_button_selectable(ss);
        endfor;

    ;;; else no top-level subsystem running
    endif;

    XtRealizeWidget(controltool_shell);
    delete_response -> XptShellDeleteResponse(controltool_shell);

    true ->> XptGarbageCursorFeedback(controltool_shell)
          -> XptBusyCursorFeedback(controltool_shell);

    XtDisplayOfObject(controltool_shell) -> controltool_display;
    if default_ss then true -> subsystem_button_set(default_ss) endif;
    controltool_shell -> pop_ui_control_panel;

    unless ss_changed_warn_set then
        ;;; Change subsystem_changed_warn so that whenever the top-level
        ;;; compiler is changed the button on the control panel is also changed
        procedure(ss_name, warn);
            lvars ss_name, warn;
            warn(ss_name);
            ss_name -> current_subsystem_button;
        endprocedure(% subsystem_changed_warn %) -> subsystem_changed_warn;
        true -> ss_changed_warn_set
    endunless;

    XtPopup(controltool_shell, XtGrabNone);
enddefine;

SET_GUI(controltool_switch_vec, controltool_xm, controltool_xol,
                                            'pop_ui_popcontroltool');

endexload_batch;
endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 22 1996
        Changed to use new external_defer_apply facility for creating
        closures automatically.
--- Robert Duncan, Jul 17 1996
        Options menu now pops up a different propsheet box for each item
--- Robert John Duncan, Jul  6 1995
        Changed delete_response to kill any current project
--- Robert John Duncan, Jul  4 1995
        Ensured that documentation on the Help menu is accessed from Pop-11
--- Robert John Duncan, Jul  3 1995
        Enabled Licence option on the Help menu
--- Integral Solutions Ltd, Jun 30 1995 (Julian Clinton)
        Added support for ProjectTool.
--- Robert John Duncan, Jun 21 1995
        Hid the Tools menu for now: demotool is no good and helptool is
        otherwise available from the Help menu, so just the librarytool
        has been added to the File menu
--- Robert John Duncan, Jun 14 1995
        Took out the File|New option.
--- Robert John Duncan, Jun  7 1995
        Fixed positioning of the Options dialog and added some extra menu
        options
--- John Williams, May 31 1995
        Replaced syspr('\n') with cucharout(`\n`) because of problems
        with pop_pr_quotes.
--- Robert John Duncan, May  4 1995
        Major rewrite to support a menu bar with several new options
--- John Gibson, Apr 14 1994
        Xpt*DeferApply -> external_defer_apply
--- John Gibson, Apr 13 1994
        Removed all Xpt*SetXtWakeups following Xpt*DeferApply (no longer
        necessary)
--- John Gibson, Feb 22 1994
        Put icon window in a top-level lvar to stop it being garbaged
        (fix for isl-fr.4543)
--- John Gibson, Feb  3 1994
        Moved setting of shell delete response for OLIT into guiRealizeWidget
        (where it's supposed to be!)
--- Julian Clinton, Feb  2 1994
        Control tool now ignores quit request.
--- John Gibson, Dec 15 1993
        Changed to use XptWMShellCoords to assign x, y position
--- John Gibson, Jul 14 1993
        Fixed derivation of default_ss in pop_ui_popcontroltool so it uses
        subsystem when being called inside syssetup
--- John Gibson, Jun 28 1993
        Changes for POPC
Julian Clinton, 21/10/91
    Renamed -pop_control_panel- to -pop_ui_control_panel-.
--- Robert John Duncan, Sep 27 1991
        Exported -pop_control_panel-
--- Integral Solutions Ltd, Sep 12 1991 (Julian Clinton)
    Removed true assignment to -pop_record_writeable-.
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    popmemlim set according to popmemused.
Julian Clinton,  20/8/91
    Changed mishap to a warning if widget set cannot be determined.
Julian Clinton,  15/7/91
    Changed to use XOPENLOOK instead of XOPENWINDOWS.
 */
