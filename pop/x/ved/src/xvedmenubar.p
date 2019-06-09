/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/ved/src/xvedmenubar.p
 > Purpose:         Motif and OpenLook menubars for XVed
 > Author:          Jonathan Meyer, Jun 17 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section $-xved => xvedmenubar;

#_INCLUDE 'gui.ph'
include xved_constants.ph;
include vedfile_struct.ph;
include xpt_xtypes.ph;

uses
;;; vedxvedmenu,        ;;; ved default menu binding
    vedxvedmenubar,     ;;; ved default menubar binding
;

vars
    xvedpopupmenushell  = false,    ;;; Popup menu shell widget
    xvednewmenubar      = false,
;

lvars
    popup_menupane = false,         ;;; Popup menu pane
    popup_buttons = [],             ;;; buttons on popup menu
    popup_spec = false,             ;;; current popup window spec

    buffer_menushell = false,       ;;; Buffer menu shell
    buffer_menupane = false,        ;;; Buffer menu pane
    buffer_labels = [],             ;;; strings for buffer labels
    buffer_buttons = [],            ;;; widgets for buffer labels
    buffer_button_count = 0,        ;;; current number of buffer labels
    buffer_blank_button = false,    ;;; Button shown when there are 0 buffers

    ;;; list of top level (pinnable) menupanes
    stayup_menupanes = [],

    menubar_gui_switch_vec,
;

;;; event handler for gotoWindow
procedure(w, name, data) -> (w, name, data);
    lvars w, name, data;
    xved_select_window(w);
endprocedure -> xvedeventtable("gotoWindow");

;;; event handler for menuAction
procedure(w, name, call) -> (w, name, call);
    lvars w, call, name;
    define lconstant Deref_arg(arg);
        lvars arg val;
        if arg.isvector then appdata(arg, Deref_arg); return;
        elseif arg.isword then caller_valof(arg,false) -> arg;
        elseif arg.isident then idval(arg) -> arg;
        endif;
        if arg.isstring then veddo(arg,not(vedonstatus)) else apply(arg); endif;
    enddefine;
    xved_select_window(w);
    Deref_arg(call);
endprocedure -> xvedeventtable("menuAction");

define lconstant Remove_dead_children(list);
    lvars list, item;
    [%  fast_for item in list do
            if XptIsLiveType(item, "Widget") then item endif;
        endfor
    %];
    sys_grbg_list(list);
enddefine;

define :inline lconstant CHECK_STAYUP_MENUPANES;
    if b_HAS_STAYUP_MENUS then
        Remove_dead_children(stayup_menupanes) -> stayup_menupanes;
    endif;
enddefine;

define lconstant Destroy_buffer_menu;
    if buffer_menupane.isXptDescriptor then
        fast_XtDestroyWidget(buffer_menupane);
        false -> buffer_menupane;
        sys_grbg_list(buffer_labels);
        sys_grbg_list(buffer_buttons);
        0 -> buffer_button_count; [] ->> buffer_labels -> buffer_buttons;
        false -> buffer_blank_button;
    endif;
    CHECK_STAYUP_MENUPANES();
enddefine;

define lconstant Destroy_popup_menu;
    if xvedpopupmenushell.isXptDescriptor then
        fast_XtDestroyWidget(xvedpopupmenushell);
    endif;
    false ->> xvedpopupmenushell ->> popup_menupane -> popup_spec;
    Destroy_buffer_menu();
enddefine;

/* Raw menu creation procedure */

define lconstant New_button(parent, label, cbproc, value) -> widget;
    lvars parent, label, cbproc, value, widget;
    p_NEW_BUTTON(parent, label) -> widget;
    if value then
        fast_XtAddCallback(widget, n_ActivateCallback,
                    XptExportTypedCallback(cbproc, value, identfn, false));
    else
        fast_XtSetSensitive(widget, false);
    endif;
enddefine;

define lconstant Build_menubutton(parent, specs, xvwin, fg, bg) -> button;
    lvars   parent, specs, xvwin, spec, pane, button, name, value,
            haspin, fg, bg;

    ;;; Callback for pane option
    define lconstant Button_cb(w, client, call);
        lvars w, client, call, xvwin;
        if client.ispair then
            ;;; from a button on a menubar
            fast_destpair(client) -> client
        else
            ;;; from the global popup pane
            wvedwindow
        endif -> xvwin;
        if wved_is_live_window(xvwin) then
            xved_raise_event(xvwin, "menuAction", client);
        endif;
    enddefine;

    xved_set_color_args(fg, bg, true);

    if ispair(specs) then
        ;;; sub-menu
        dest(specs) -> (name, specs);
        lmember("fixable", specs) -> haspin;

        ;;; Make a blank menubutton and pane
        p_NEW_MENUBUTTON(parent, name, haspin) -> (button, pane);
        if b_HAS_STAYUP_MENUS and haspin then
            pane :: stayup_menupanes -> stayup_menupanes;
        endif;

        if not(xvwin) and name = XVMB_GOTOBUFF_LABEL then
            Destroy_buffer_menu(); pane -> buffer_menupane;
        endif;

        ;;; Make each of the items on the pane
        for spec in specs do
            unless spec == "fixable" then
                Build_menubutton(pane, spec, xvwin, fg, bg) ->
            endunless
        endfor

    elseif isvector(specs) and datalength(specs) == 2 then
        ;;; action
        explode(specs) -> (name, value);
        if xvwin then conspair(xvwin, value) -> value endif;
        New_button(parent, name, Button_cb, value) -> button
    elseif specs == "space" then
        ;;; separator
        fast_XtCreateManagedWidget(nullstring, wc_BLANK, parent,
                                                xved_arg_list()) -> button
    else
        mishap(specs, 1, 'BAD MENU SPECIFICATION')
    endif
enddefine;

/* Modify an old menu structure to correspond to a new one */
define lconstant Rebuild_menu(xvwin, menubar, old_specs, old_children,
                                                    new_specs, fg, bg);
    lvars   xvwin, menubar, old_specs, old_children, new_specs, spec,
            count = 0, w, fg, bg;
    {%  fast_for spec in new_specs do
            if old_specs and old_specs.islist and old_specs /== [] then
                count fi_+ 1 -> count;
                if (dest(old_specs) -> old_specs) = spec then
                    ;;; keep original menu
                    old_children(count); false -> old_children(count);
                    nextloop;
                else
                    ;;; when first old menu doesn't match, all menus are
                    ;;; rebuilt.
                    sys_grbg_list(old_specs); false -> old_specs;
                endif;
            endif;
            Build_menubutton(menubar, spec, xvwin, fg, bg)
        endfor
    %};     ;;; -> new_children

    ;;; clean up any leftovers
    if old_children.isvector then
        fast_for w in_vector old_children do
            if w then fast_XtDestroyWidget(w) endif;
        endfor;
    endif;
    CHECK_STAYUP_MENUPANES();
enddefine;

;;; called to reset labels on the buffermenu
define xved_check_buffer_menu();
    lvars count, num, w, labels, label;
    lconstant name_to_window_map = newanyproperty([], 16, false, false,
                                    syshash, nonop =, "tmpval", false, false);

    define :inline lconstant NEXT(list);
        (sys_grbg_destpair(list) -> list)
    enddefine;

    ;;; Generate labels for files on the 'Buffer' menu
    define lconstant Buffmenu_label(file) -> name;
        lvars file, win, name;
        if file == ved_current_file then
            wvedwindow, vednamestring
        else
            fast_subscrv(VF_WINDOW, file), fast_subscrv(VF_NAMESTRING, file)
        endif -> (win, name);
        allbutfirst(locchar(`\s`,1,name), name) -> name;
        win -> name_to_window_map(name);
    enddefine;

    define lconstant Select_cb(w, client, call);
        lvars w, client, call;
        if name_to_window_map(p_LABEL_OF(w)) ->> w then
            xved_raise_event(w, "gotoWindow", false)
        endif
    enddefine;

    returnunless(XptIsLiveType(buffer_menupane, "Widget"));

    listlength(vedbufferlist) -> count;
    count fi_- buffer_button_count -> num;
    count -> buffer_button_count;
    ;;; make sure we have enough buttons
    if num fi_< 0 then
        fast_repeat -num times
            NEXT(buffer_buttons) -> w;
            fast_XtDestroyWidget(w);
        endrepeat;
    elseif num fi_> 0 then
        fast_repeat num times
            New_button(buffer_menupane, nullstring, Select_cb, true) -> w;
            conspair(w, buffer_buttons) -> buffer_buttons;
        endrepeat;
    endif;

    if count == 0 then
        New_button(buffer_menupane, '-none-', "dummy", false)
            -> buffer_blank_button;
    else
        if buffer_blank_button then
            XtDestroyWidget(buffer_blank_button);
            false -> buffer_blank_button;
        endif;
    endif;

    ;;; set button labels
    clearproperty(name_to_window_map);
    maplist(vedbufferlist, Buffmenu_label) -> labels;
    syssort(labels, false, #_< alphabefore <> not >_#) -> labels;

    fast_for label, w in labels, buffer_buttons do
        nextif(label = p_LABEL_OF(w));
        label -> p_LABEL_OF(w);
    endfor;
    sys_grbg_list(buffer_labels);
    labels -> buffer_labels;
enddefine;

define xved_menubar_children(widget);
    lvars widget, pane;
    if p_HAS_MENUBAR_MENU(widget) ->> pane then pane endif
enddefine;

define xved_init_menubar(window);
    lvars   window, new_spec, old_spec = false, menubar, new_children,
            old_children = false, nrows, ncolumns, fg, bg;

    xvednewmenubar or xved_default_window_value("menubar") -> new_spec;
    unless islist(new_spec) and new_spec /== [] then
        false -> new_spec
    endunless;

    if isvector(window.xvedwin_menubar) then
        ;;; get the previous menubar
        explode(window.xvedwin_menubar) -> (menubar, old_spec, old_children)
    endif;

    unless new_spec then
        ;;; kill previous menubar
        if old_spec then XtDestroyWidget(menubar) endif;
        xved_init_menubar -> window.xvedwin_menubar;
        return
    endunless;

    XptVal[fast] window(XtN menubarForeground:XptPixel,
                        XtN menubarBackground:XptPixel) -> (fg, bg);

    unless old_spec then
        ;;; create new menubar - we want to preserve the window's text
        ;;; size, but the size of the shell may increase to include the menubar
        XptVal[fast] window(XtN numColumns, XtN numRows) -> (ncolumns, nrows);
        xved_set_color_args(fg, bg, true);
        xved_set_args(XtN borderWidth, 0, false);
        p_NEW_MENUBAR(XtParent(window), window) -> menubar;
        XtManageChild(menubar)
    endunless;

    procedure;
        if old_spec then
            ;;; changing existing menubar should not alter size of a window
            dlocal %XptVal[fast] (window.xvedwin_shell)
                        (XtN allowShellResize:XptBoolean)% = false;
        endif;

        Rebuild_menu(window, menubar, old_spec, old_children, new_spec, fg, bg)
            -> new_children;

    endprocedure();

    {% menubar, copylist(new_spec), new_children %} ->  window.xvedwin_menubar;

    unless old_spec then
        ;;; this is a new menubar - make sure that the windows size text
        ;;; area remains the same size
        ncolumns, nrows -> XptVal window(XtN numColumns, XtN numRows);
    endunless;
enddefine;

    /*  button is false for initialising only.
    */
define xved_popup_menu(button);
    lvars button, popupmenu, waslive, fg, bg;

    define lconstant convert_colour(c) -> c;
        if isstring(c) then
            c -> XptVal[fast] xveddummyshell(XtN background:XptPixel
                                                    <TYPE=string>);
            XptVal[fast] xveddummyshell(XtN background:XptPixel) -> c
        endif
    enddefine;

    returnunless(xvedappcontext);
    xved_value("application", "Menu") -> popupmenu;
    unless popupmenu.islist then
        Destroy_popup_menu();
        return;
    endunless;
    XptIsLiveType(xvedpopupmenushell, "Widget") -> waslive;
    unless waslive and button then
        ;;; rebuild it
        xved_value("application", [MenuForeground MenuBackground]) -> (fg, bg);
        convert_colour(fg) -> fg;
        convert_colour(bg) -> bg;
        unless waslive then
            xved_set_color_args(fg, bg, true);
            p_NEW_MENUSHELL(xveddummyshell, 'Ved Menu', true)
                            -> (xvedpopupmenushell, popup_menupane);
            false -> popup_spec;
            [] -> popup_buttons
        endunless;
        if b_HAS_STAYUP_MENUS then
            popup_menupane :: stayup_menupanes -> stayup_menupanes;
        endif;
        Rebuild_menu(false, popup_menupane, popup_spec, popup_buttons,
                                    popupmenu, fg, bg) -> popup_buttons;
        copylist(popupmenu) -> popup_spec;
        xved_check_buffer_menu()
    endunless;
    if button then
        p_POPUP_MENU(xvedpopupmenushell, wvedwindow, button)
    endif
enddefine;


/* Tidy up */

lvars setup_done = false;

;;; disable any pinned menus
procedure;
    lvars mw;
    returnunless(b_HAS_STAYUP_MENUS);
    fast_for mw in stayup_menupanes do
        if XptIsLiveType(mw, "Widget") and XtIsRealized(mw)
        and XptVal[fast] mw(XtN numChildren) /== 0 then
            fast_XtSetSensitive(mw, false);
        endif;
    endfor;
endprocedure <> xved_before_editor_exit -> xved_before_editor_exit;

procedure;
    lvars mw;
    unless setup_done then
        unless xvedvanilla then
            ;;; initialise popup menu the first time
            xved_popup_menu(false)
        endunless;
        true -> setup_done
    endunless;
    returnunless(b_HAS_STAYUP_MENUS);
    fast_for mw in stayup_menupanes do
        if XptIsLiveType(mw, "Widget") and XtIsRealized(mw)
        and XptVal mw(XtN numChildren) /== 0 then
            fast_XtSetSensitive(mw, true);
        endif;
    endfor;
endprocedure <> xved_after_editor_entry -> xved_after_editor_entry;


/* ======= Set for Motif or OLIT ====================================== */

#_IF not(DEF POPC_COMPILING)
    #_IF DEF popxlink_motif
        uses $-xved$-menubar_xm;
        weak constant menubar_xol;
    #_ELSEIF DEF popxlink_openlook
        uses $-xved$-menubar_xol;
        weak constant menubar_xm;
    #_ENDIF
#_ENDIF

define lconstant set_gui();
    if testdef popxlink_motif then
        weakref[popxlink_motif] menubar_xm
    elseif testdef popxlink_openlook then
        weakref[popxlink_openlook] menubar_xol
    else
        mishap(0, 'XVed: SYSTEM NOT LINKED WITH MOTIF OR OPENLOOK');
    endif -> menubar_gui_switch_vec
enddefine;

#_IF DEF POPC_COMPILING
    sys_runtime_apply(set_gui);
#_ELSE
    set_gui();
#_ENDIF

constant xvedmenubar = true;    ;;; signature that menubar is loaded

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Oct  9 1997
        Changed New_button to call p_NEW_BUTTON so that the button label
        can be Unicode
--- John Gibson, Nov 22 1996
        Fixed xved_popup_menu when MenuForeground/MenuBackground are strings.
--- John Gibson, Feb 28 1994
        Rewrote menubutton-building code so it allows top-level entries to be
        action buttons not just sub-menu buttons
--- John Gibson, Jan  3 1994
        Set borderWidth 0 on menubar
--- John Gibson, Dec 17 1993
        Made xved_popup_menu use MenuForeground/Background application
        resources
--- John Gibson, Jun  3 1993
        Rewritten for POPC etc
--- Adrian Howard, Mar 22 1993
        o Changed to use #_TERMIN_IF
        o Now changes fontColor under OLIT
--- John Gibson, Dec 19 1992
        Added xved_menubar_children, removed xved_ch*ildren_of_widget
--- John Gibson, Sep  9 1992
        Changed to use XptVal
--- Adrian Howard, Aug 17 1992
        -HAS_PANE- renamed -HAS_MENUBAR_MENU-
--- Adrian Howard, Aug 14 1992
        Added -xved_children_of_widget-
--- Adrian Howard, Aug 13 1992
        o Made color of menus the same as the color of the menubar
        o Made the buttons on the Menubar have the menubarFore/Background colours
--- John Gibson (pp Jonathan Meyer), Aug  8 1992
       o made menuAction only place the command on the status line if the
         user is not currently on the status line (veddo
         called with not(vedonstatus)).
       o added call to check buffer menu at end of xved_popup_menu.
         Makes certain that the buffer menu is correct if ved us running when
         the popup menu is created.
--- John Gibson, Jul 28 1992
        Set menubar fg/bg from Text widget resources
--- Jonathan Meyer, Sep 16 1991 changed pushpin to fixable
--- Jonathan Meyer, Sep 11 1991
        Rewrote. Now uses xved_gui.ph to hold Motif/OpenLook definitions.
        Also allows users to change both the popup and the menubar menus.
        Changed buffermenu so that <ENTER> name updates the buffermenu
--- Jonathan Meyer, Sep  3 1991
        Changed to use a new (more compact) menu structure representation
--- John Gibson, Aug 19 1991
        vedmouse__ procedures now get args from global vars
--- John Gibson, Aug 10 1991
        Made event handlers return their args
--- Adrian Howard, Aug  9 1991 : XptCoerceCallback --> XptExportCallback
--- Jonathan Meyer, Aug  2 1991
        Changed to use XV_NO_ARGS. Now allows vectors of arguments
--- Jonathan Meyer, Aug  1 1991
        Made vedmouse__menu raise the window so that pinned windows
        become visible
--- Jonathan Meyer, Jul 27 1991
        Added xved_before_editor_exit action
--- Jonathan Meyer, Jul 27 1991
        Removed menu building banner
--- Jonathan Meyer, Jul 26 1991
        Removed menu to *vedxvedmenu.p. Fixed 'Buffer' menu for motif
--- Jonathan Meyer, Jul  8 1991
        Added test for menubarOn
--- Jonathan Meyer, Jul  8 1991
        Added several menu options
--- Jonathan Meyer, Jul  7 1991
        Allowed menu options to be idents.
--- Jonathan Meyer, Jun 24 1991
        Changed to use XptCoerceCallback
--- Jonathan Meyer, Jun 22 1991
        Removed hardwired background colouring
 */
