/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-projecttool_xol.p
 > Purpose:         Open Look project tool
 > Author:          Julian Clinton, June 1995 (see revisions)
 > Documentation:
 > Related Files:
*/

#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +strict;


section $-poplog_ui;

uses-now Xol;

include pop_uiP;
include xpt_coretypes;
include XolConstants;
include XolScroll;

uses
    sprintf,
    $-poplog_ui$-guiXlibs,
    $-poplog_ui$-guiShells,
    $-poplog_ui$-guiUtils,
    $-poplog_ui$-guiMouseEvents,
    $-poplog_ui$-guiProjectUtils,
;

uses
    xt_init,
    xt_widget,
    xt_widgetclass,
    xt_widgetinfo,
    xt_composite,
    xt_popup,
    xt_event,
    xt_callback,
    xt_resource,

    xtTopLevelShellWidget,
    xolControlAreaWidget,
    xolMenuButtonWidget,
    xolOblongButtonWidget,
    xolRectButtonWidget,
    xolTextEditWidget,
    xolTextFieldWidget,
    xolScrollingListWidget,
    xolFormWidget,
    xolStaticTextWidget,

    XptShellDeleteResponse,
    XptBusyCursorFeedback,
    XptGarbageCursorFeedback,

    xpol_listutils,
;


/* ---------------------------------------------------------------
    Local Variables and Constants
   --------------------------------------------------------------- */

lvars projecttool_icon_window;

lconstant projecttool_switch_vec = INIT_projecttool_switch_vec;
constant projecttool_xol             = projecttool_switch_vec;


defclass xol_projecttool {
    xolp_parent,
    xolp_shell,
    xolp_menubar,
    xolp_projmenu,
    xolp_selectmenu,
    xolp_editmenu,
    xolp_buildmenu,
    xolp_helpmenu,
    xolp_filelist,
    xolp_footer,
    xolp_cancelbtn,
    xolp_on_close,
};


/* ---------------------------------------------------------------
    List Selection Utilities
   --------------------------------------------------------------- */

lvars applTouchItem = false, applViewItem, applUpdateView;

define lconstant getListWidgetProcs(list); lvars list;
    XptVal[fast] list(XtN applTouchItem:XptProcedure,
                        XtN applViewItem:XptProcedure,
                        XtN applUpdateView:XptProcedure)
        -> (applTouchItem, applViewItem, applUpdateView);
enddefine;

define lconstant _doSelectItem(item, list); lvars item, list;

    lvars tag = XpolListItemToToken(list, item), litem;

    unless applTouchItem then
        getListWidgetProcs(list);
    endunless;

    if tag then
        OlListItemPointer(tag) -> litem;
        (exacc [fast] :OlListItem litem.attr) fi_|| OL_LIST_ATTR_SELECTED
                -> exacc [fast] :OlListItem litem.attr;
        ;;; exacc [fast] applTouchItem (list, tag);
        exacc applTouchItem (list, tag);
    endif;
enddefine;


define lconstant _doDeselectItem(item, list); lvars item, list;

    lvars tag = XpolListItemToToken(list, item), litem;

    if tag then
        OlListItemPointer(tag) -> litem;
        (exacc :OlListItem litem.attr) fi_&&~~
                ;;; (OL_LIST_ATTR_CURRENT || OL_LIST_ATTR_SELECTED)
                OL_LIST_ATTR_SELECTED
            -> exacc :OlListItem litem.attr;
        ;;; exacc [fast] applTouchItem (list, tag);
        exacc applTouchItem (list, tag);
    endif;
enddefine;


/* ---------------------------------------------------------------
    Displaying Info
   --------------------------------------------------------------- */

define lconstant DisplayWin(win_data); lvars win_data, shell;
    XtPopup((win_data.xolp_shell ->> shell), 0);
    erase <> win_data.xolp_on_close -> XptShellDeleteResponse(shell);

    ;;; set busy cursors for evrything except the Cancel button
    ;;;
    lvars menubar, filelist, footer;
    unless XptBusyCursorFeedback((win_data.xolp_menubar ->> menubar)) then
        true ->> XptBusyCursorFeedback(menubar)
            ->> XptBusyCursorFeedback((win_data.xolp_filelist ->> filelist))
            -> XptBusyCursorFeedback((win_data.xolp_footer ->> footer));

        true ->> XptGarbageCursorFeedback(menubar)
            ->> XptGarbageCursorFeedback(filelist)
            ->> XptGarbageCursorFeedback(win_data.xolp_cancelbtn)
            -> XptGarbageCursorFeedback(footer);
    endunless;
enddefine;


define lconstant RaiseWin(win_data); lvars win_data;
    XRaiseWindow(XtDisplayOfObject(win_data.xolp_shell),
                XtWindow(win_data.xolp_shell));
enddefine;


define lconstant CloseWin(win_data); lvars win_data;
    XtPopdown(win_data.xolp_shell);
enddefine;

define lconstant SetShellPos(x, y, width, height, win_data);
    lvars x, y, width, height, win_data, shell = win_data.xolp_shell;
    x, y, width, height -> XptWidgetCoords(shell);
enddefine;


define lconstant SetTitle(title, win_data);
    lvars win_data, title, shell = win_data.xolp_shell;
    sprintf(title, 'Poplog: Project %S') -> XptVal shell(XtN title:XptString);
    title -> XptVal shell(XtN iconName:XptString);
enddefine;

define lconstant SetFooter(msg, win_data);
lvars msg, win_data, footer = win_data.xolp_footer, tedit;
    OlTextEditClearBuffer(XptVal footer(XtN textEditWidget:XptWidget)
        ->> tedit)->;
    OlTextEditInsert(tedit, msg, length(msg)) ->;
enddefine;

;;; have to use space rather than nullstring to clear garbage from window
define lconstant ClearFooter(win_data);
lvars win_data, footer = win_data.xolp_footer;
    OlTextEditClearBuffer(XptVal footer(XtN textEditWidget:XptWidget))->;
enddefine;

define lconstant GetList(win_data);
    lvars win_data;
    expandlist(XpolListItems(win_data.xolp_filelist));
enddefine;

define lconstant GetListCount(win_data);
    lvars win_data, l;
    listlength(XpolListItems(win_data.xolp_filelist) ->> l);
    sys_grbg_list(l);
enddefine;

define lconstant GetSelection(win_data);
    lvars win_data;
    /*NOTSUPPORTED
    expandlist(XpolSelectedListItems(win_data.xolp_filelist));
    */
    ;;; single selection only
    lvars current = XpolCurrentListItem(win_data.xolp_filelist);
    [% if current then current endif %];
enddefine;

define lconstant GetSelectCount(win_data);
    lvars win_data, l;
    /*NOTSUPPORTED
    listlength(XpolSelectedListItems(win_data.xolp_filelist) ->> l);
    sys_grbg_list(l);
    */
    ;;; single selection only
    XpolCurrentListItem(win_data.xolp_filelist) and 1 or 0;
enddefine;

define lconstant SetList(string_list, win_data);
    lvars string_list, win_data;
    string_list -> XpolListItems(win_data.xolp_filelist);
enddefine;

define lconstant ReplaceItem(new_item, n, win_data);
    lvars new_item, n, win_data;
    new_item -> XpolSubscrListItems(n, win_data.xolp_filelist);
enddefine;


/*NOTSUPPORTED
define lconstant SetSelection(string_list, win_data);
    lvars string_list, win_data;

    ;;; xpol_listutils can't do this yet...
    ;;;     string_list -> XpolSelectedListItems(win_data.xolp_filelist);
    ;;; ...so do it ourselves
    ;;;
    lvars list = win_data.xolp_filelist, item;

    unless applTouchItem then
        getListWidgetProcs(list);
    endunless;

    exacc applUpdateView(list, false);
    for item in string_list do
        _doSelectItem(item, list);
    endfor;
    exacc applUpdateView(list, true);
enddefine;
*/

/*NOTSUPPORTED
define lconstant SelectAll(win_data);
    lvars win_data, list = win_data.xolp_filelist;

    lvars item;

    unless applTouchItem then
        getListWidgetProcs(list);
    endunless;

    exacc applUpdateView(list, false);
    for item in XpolListItems(list) do
        _doSelectItem(item, list);
    endfor;
    exacc applUpdateView(list, true);
enddefine;
*/

/*NOTSUPPORTED
define lconstant DeselectAll(win_data);
    lvars win_data, list = win_data.xolp_filelist;

    lvars current = XpolCurrentListItem(list);
    lvars item;

    unless applTouchItem then
        getListWidgetProcs(list);
    endunless;

    exacc applUpdateView(list, false);
    for item in XpolListItems(list) do
        unless item = current then
            _doDeselectItem(item, list);
        endunless;
    endfor;
    exacc applUpdateView(list, true);
enddefine;
*/

define lconstant GetCancelState(win_data);
lvars win_data, btn = win_data.xolp_cancelbtn;
    XptVal btn(XtN sensitive:XptBoolean);
enddefine;

define lconstant SetCancelState(state, win_data);
lvars state, win_data, btn = win_data.xolp_cancelbtn;

    ;;; if it's sensitive, make sure it's not blocked by the busy cursor
    ;;;
    not(state) -> XptBusyCursorFeedback(btn);
    state -> XptVal btn(XtN sensitive:XptBoolean);
enddefine;


/* ---------------------------------------------------------------
    Utilities
   --------------------------------------------------------------- */

define :macexpr p_PROJECTTOOL_OP(win_data, op);
lvars win_data op;

    go_on op to DISPLAY RAISE CLOSE SETSHELLPOS SETTITLE SETFOOTER CLEARFOOTER
        GETLIST GETLISTCOUNT GETSELECTION GETSELECTCOUNT
        SETLIST SETSELECTION SELECTALL DESELECTALL
        REPLACE ADDITEMS REMOVEITEMS GETMENUBAR GETMENUS GETSHELL GETPARENT
        GETCANCELSTATE SETCANCELSTATE;

    DISPLAY:
        DisplayWin(win_data);
        return;

    RAISE:
        RaiseWin(win_data);
        return;

    CLOSE:
        CloseWin(win_data);
        return;

    SETSHELLPOS:
        SetShellPos(win_data);
        return;

    SETTITLE:
        SetTitle(win_data);
        return;

    SETFOOTER:
        SetFooter(win_data);
        return;

    CLEARFOOTER:
        ClearFooter(win_data);
        return;

    GETLIST:
        GetList(win_data);
        return;

    GETLISTCOUNT:
        GetListCount(win_data);
        return;

    GETSELECTION:
        GetSelection(win_data);
        return;

    GETSELECTCOUNT:
        GetSelectCount(win_data);
        return;

    SETLIST:
        SetList(win_data);
        return;

    SETSELECTION:
        /*NOTSUPPORTED
        SetSelection(win_data);
        */
        return;

    SELECTALL:
        /*NOTSUPPORTED
        SelectAll(win_data);
        */
        return;

    DESELECTALL:
        /*NOTSUPPORTED
        DeselectAll(win_data);
        */
        return;

    REPLACE:
        ReplaceItem(win_data);
        return;

    ADDITEMS:
        return;

    REMOVEITEMS:
        return;

    GETMENUBAR:
        win_data.xolp_menubar;
        return;

    GETMENUS:
        win_data.xolp_projmenu;
        win_data.xolp_selectmenu;
        win_data.xolp_editmenu;
        win_data.xolp_buildmenu;
        win_data.xolp_helpmenu;
        return;

    GETSHELL:
        win_data.xolp_shell;
        return;

    GETPARENT:
        win_data.xolp_parent;
        return;

    GETCANCELSTATE:
        GetCancelState(win_data);
        return;

    SETCANCELSTATE:
        SetCancelState(win_data);
        return;
enddefine;


/* ---------------------------------------------------------------
    Creation Routines
   --------------------------------------------------------------- */

define lconstant NotifyClose(w, win_data, call);
    lvars w, win_data, call;

    ;;; check if some user procedure has been passed and if so
    ;;; apply it
    ;;;
    if isprocedure(win_data.xolp_on_close) then
        XptDeferApply(win_data.xolp_on_close);
        XptSetXtWakeup();
    endif;
enddefine;


define lconstant add_menu_button(parent, name, label, default, client, cb);
    lvars parent, name, label, default, client, cb, button;

    if label = nullstring then
        XtVaCreateManagedWidget(label, xolStaticTextWidget, parent,
                    #| |#) -> ;
    else
        XtVaCreateManagedWidget(name, xolOblongButtonWidget, parent,
                    #|
                        XtN default, default,
                    |#) -> button;
        label -> XptVal button(XtN label:XptString);
        XtAddCallback(button, XtN select, cb, client);
    endif;
enddefine;



define :macexpr p_PROJECTTOOL(projname,
    proj_desc, select_desc, edit_desc, build_desc, help_desc,
    menuprepare_cb, generic_list_cb, selection_code, action_code,
    on_close, on_cancelbtn, ref_widget) -> projtool;

lvars projname,
    proj_desc, select_desc, edit_desc, build_desc, help_desc,
    menuprepare_cb, generic_list_cb, selection_code, action_code,
    on_close, on_cancelbtn, ref_widget, projtool;

    define lconstant create_menu_buttons(parent, bg, desc, pre_cb, prevbutton)
                        -> menu -> button;
    lvars parent, bg, desc, pre_cb, prevbutton, menu, button;

        XtVaCreateManagedWidget(subscrv(1, desc),
                xolMenuButtonWidget, parent, 0) -> button;

        XptVal button(XtN menuPane:XptWidget) -> menu;

        if prevbutton then
            prevbutton, true, 8 -> XptVal button(XtN xRefWidget:XptWidget,
                        XtN xAddWidth:XptBoolean, XtN xOffset);
        endif;

        if pre_cb then
            XtAddCallback(XptShellOfObject(menu), XtN popupCallback, pre_cb, menu);
        endif;
    enddefine;


    define lconstant add_menu_buttons(desc, menu);
    lvars desc, menu, menu_cb, button;

        subscrv(3, desc) -> menu_cb;
        fast_for button in_vector desc do
            if isvector(button) then
                add_menu_button(menu, explode(button), menu_cb)
            endif;
        endfast_for;
    enddefine;


    lvars last_clicktime = 0, last_selection = nullstring;

    define lconstant list_cb(w, client, call, generic_cb);
    lvars w, client, call, generic_cb, seln;
        call -> XpolCurrentListItem(w);
        XpolListTokenToItem(call) -> seln;
        if (is_doubleclick(XptDefaultDisplay, last_clicktime)
                -> last_clicktime) and seln = last_selection then
            false -> last_selection;
            generic_cb(client, seln);
        else
            seln -> last_selection;
        endif;
    enddefine;

    consxol_projecttool(dupnum(false, 12)) -> projtool;

    ref_widget -> projtool.xolp_parent;
    on_close -> projtool.xolp_on_close;

    lconstant bitmap_base_name = '$usepop/pop/x/ui/bitmaps/projecttool';
    xved_create_icon_window(
            xved_get_icon_filename(ref_widget, bitmap_base_name),
            false) -> projecttool_icon_window;

    lvars shell;
    XtVaCreatePopupShell('projectTool', xtTopLevelShellWidget, ref_widget,
        #|
            XtN title, sprintf(projname, 'Poplog: Project %p'),
            XtN iconWindow, projecttool_icon_window,
            XtN iconName, projname,
        |# ) ->> shell -> projtool.xolp_shell;

    lvars form = XtVaCreateManagedWidget('form', xolFormWidget, shell, 0);

    ;;; I can't get the form to put space at the top and left, even when
    ;;; xAttachOffset/yAttachOffset are set, so this invisible widget
    ;;; does the job instead
    lvars pad = XtVaCreateManagedWidget('pad', xolStaticTextWidget,
        form, (#|
            XtN width,              0,
            XtN height,             0,
            XtN borderWidth,        0,
            XtN hSpace,             0,
            XtN vSpace,             0,
            XtN recomputeSize,      false,
            XtN selectable,         false,
            XtN traversalOn,        false,
        |#));

    lvars menubar = XtVaCreateManagedWidget('menubar', xolFormWidget,
        form, (#|
            XtN xRefWidget,         pad,
            XtN xOffset,            5,
            XtN xAttachRight,       true,
            XtN xAttachOffset,      5,
            XtN xResizable,         true,
            XtN yRefWidget,         pad,
            XtN yOffset,            5,
            XtN yResizable,         false,
        |#));
    menubar -> projtool.xolp_menubar;

    lvars bg = XptVal menubar(XtN background);

    lvars proj_menu, proj_widget;
    create_menu_buttons(menubar, bg, proj_desc, menuprepare_cb, false)
        ->> proj_menu -> projtool.xolp_projmenu
        -> proj_widget;

    lvars select_menu, select_widget;
    create_menu_buttons(menubar, bg, select_desc, menuprepare_cb, proj_widget)
        ->> select_menu -> projtool.xolp_selectmenu
        -> select_widget;

    lvars edit_menu, edit_widget;
    create_menu_buttons(menubar, bg, edit_desc, menuprepare_cb, select_widget)
        ->> edit_menu -> projtool.xolp_editmenu
        -> edit_widget;

    lvars build_menu, build_widget;
    create_menu_buttons(menubar, bg, build_desc, menuprepare_cb, edit_widget)
        ->> build_menu -> projtool.xolp_buildmenu
        -> build_widget;

    lvars help_menu, help_widget;
    create_menu_buttons(menubar, bg, help_desc, menuprepare_cb, build_widget)
        ->> help_menu -> projtool.xolp_helpmenu
        -> help_widget;

    (true, true) -> XptVal help_widget(XtN xAttachRight:XptBoolean,
        XtN xVaryOffset:XptBoolean);

    ;;; add the menu contents
    ;;;
    add_menu_buttons(proj_desc, proj_menu);
    add_menu_buttons(select_desc, select_menu);
    add_menu_buttons(edit_desc, edit_menu);
    add_menu_buttons(build_desc, build_menu);
    add_menu_buttons(help_desc, help_menu);

    lvars list = XtVaCreateManagedWidget('list', xolScrollingListWidget,
        form, (#|
            XtN recomputeWidth,     true,
            XtN viewHeight,         4,
            XtN selectable,         false,
            XtN xRefWidget,         pad,
            XtN xOffset,            5,
            XtN xAttachRight,       true,
            XtN xAttachOffset,      5,
            XtN xResizable,         true,
            XtN yRefWidget,         menubar,
            XtN yOffset,            5,
            XtN yAddHeight,         true,
            XtN yResizable,         true,
        |#));
    list -> projtool.xolp_filelist;

    lvars footer_form = XtVaCreateManagedWidget('form', xolFormWidget,
        form, (#|
            XtN xRefWidget,         pad,
            XtN xOffset,            5,
            XtN xAttachRight,       true,
            XtN xAttachOffset,      5,
            XtN xResizable,         true,
            XtN yRefWidget,         list,
            XtN yOffset,            5,
            XtN yAddHeight,         true,
            XtN yAttachBottom,      true,
            XtN yAttachOffset,      5,
            XtN yResizable,         false,
        |#));

    lvars footer = XtVaCreateManagedWidget('status', xolTextFieldWidget,
        footer_form, (#|
            XtN charsVisible,       50,
            XtN traversalOn,        false,
            XtN xResizable,         true,
            XtN yResizable,         false,
        |#));
    footer -> projtool.xolp_footer;

    lvars tedit = XptVal footer(XtN textEditWidget:XptWidget);
    1, 1, 1, 1, OL_TEXT_READ -> XptVal tedit(XtN topMargin, XtN leftMargin,
            XtN rightMargin, XtN bottomMargin, XtN editType);

    ClearFooter(projtool);

    lvars cancel_button = XtVaCreateManagedWidget('cancelButton',
        xolOblongButtonWidget, footer_form, (#|
            XtN sensitive,          false,
            XtN xRefWidget,         footer,
            XtN xOffset,            5,
            XtN xAddWidth,          true,
            XtN xResizable,         false,
            XtN xAttachRight,       true,
            XtN yResizable,         false,
        |#));
    cancel_button -> projtool.xolp_cancelbtn;

    'Cancel' -> XptVal cancel_button(XtN label:XptString);
    false -> XptVal cancel_button(XtN xResizable:XptBoolean);

    lvars cb_pdr;
    list_cb(%generic_list_cb%) -> cb_pdr;

    XtAddCallback(list, XtN userMakeCurrent, cb_pdr, action_code);

    if isprocedure(on_cancelbtn) then
        XtAddCallback(cancel_button, XtN select, on_cancelbtn, false);
    endif;

    false -> XptVal shell(XtN allowShellResize:XptBoolean);

    XtAddCallback(shell, XtN popdownCallback, NotifyClose, projtool);
enddefine;


endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 10 1995
        Disabled (temporarily?) everything to do with multiple selection
        (all such fragments marked NOTSUPPORTED) because the old ScrollingList
        API just doesn't behave in the way the project tool expects, i.e. like
        Motif. Changed to use single-selection instead.
        Fixed list_cb to recognise doubleclick.
        Expanded lists returned by Xpol*ListItems procedures because the
        project tool doesn't expect dynamic lists.
        Changed the title and layout of the project window.
 */
