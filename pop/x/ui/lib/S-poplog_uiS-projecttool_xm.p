/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-projecttool_xm.p
 > Purpose:         Motif project tool
 > Author:          Julian Clinton, April 1995 (see revisions)
 > Documentation:
 > Related Files:
*/

#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +strict;

section $-poplog_ui;

uses-now Xm;

include pop_uiP;
include xpt_coretypes.ph;
include XmConstants.ph;

uses
    sprintf,
    $-poplog_ui$-guiXlibs,
    $-poplog_ui$-guiUtils,
    $-poplog_ui$-guiShells,
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
    xmTextFieldWidget,
    xmListWidget,
    xmFormWidget,
    xmRowColumnWidget,
    xmCascadeButtonWidget,
    xmPushButtonWidget,
    xmSeparatorWidget,

    XptShellDeleteResponse,
    XptBusyCursorFeedback,
    XptGarbageCursorFeedback,

    xpm_listutils,
;


/* ---------------------------------------------------------------
    Local Variables and Constants
   --------------------------------------------------------------- */

lvars projecttool_icon_window;

lconstant projecttool_switch_vec = INIT_projecttool_switch_vec;
constant projecttool_xm          = projecttool_switch_vec;


defclass xm_projecttool {
    xmp_parent,
    xmp_shell,
    xmp_menubar,
    xmp_projmenu,
    xmp_selectmenu,
    xmp_editmenu,
    xmp_buildmenu,
    xmp_helpmenu,
    xmp_filelist,
    xmp_footer,
    xmp_cancelbtn,
    xmp_on_close,
};


/* ---------------------------------------------------------------
    Displaying Info
   --------------------------------------------------------------- */

define lconstant DisplayWin(win_data); lvars win_data, shell;
    XtPopup((win_data.xmp_shell ->> shell), 0);
    erase <> win_data.xmp_on_close -> XptShellDeleteResponse(shell);

    ;;; set busy cursors for evrything except the Cancel button
    ;;;
    lvars menubar, filelist, footer;
    unless XptBusyCursorFeedback((win_data.xmp_menubar ->> menubar)) then
        true ->> XptBusyCursorFeedback(menubar)
            ->> XptBusyCursorFeedback((win_data.xmp_filelist ->> filelist))
            ->> XptBusyCursorFeedback(XtParent(filelist))
            -> XptBusyCursorFeedback((win_data.xmp_footer ->> footer));

        true ->> XptGarbageCursorFeedback(menubar)
            ->> XptGarbageCursorFeedback(filelist)
            ->> XptGarbageCursorFeedback(XtParent(filelist))
            ->> XptGarbageCursorFeedback(win_data.xmp_cancelbtn)
            -> XptGarbageCursorFeedback(footer);
    endunless;
enddefine;


define lconstant RaiseWin(win_data); lvars win_data;
    XRaiseWindow(XtDisplayOfObject(win_data.xmp_shell),
                XtWindow(win_data.xmp_shell));
enddefine;


define lconstant CloseWin(win_data); lvars win_data;
    XtPopdown(win_data.xmp_shell);
enddefine;

define lconstant SetShellPos(x, y, width, height, win_data);
    lvars x, y, width, height, win_data, shell = win_data.xmp_shell;
    x, y, width, height -> XptWidgetCoords(shell);
enddefine;


define lconstant SetTitle(title, win_data);
    lvars win_data, title, shell = win_data.xmp_shell;
    sprintf(title, 'Poplog: Project %S') -> XptVal shell(XtN title:XptString);
    title -> XptVal shell(XtN iconName:XptString);
enddefine;

define lconstant SetFooter(msg, win_data);
    lvars win_data, msg, footer = win_data.xmp_footer;
    XptCheckString(msg) -> XptVal footer(XtN value:XptString);
enddefine;

;;; have to use space rather than nullstring to clear garbage from window
define lconstant ClearFooter(w); lvars w;
    SetFooter(' ', w)
enddefine;

define lconstant GetList(win_data);
    lvars win_data;
    XpmListItems(win_data.xmp_filelist);
enddefine;

define lconstant GetListCount(win_data);
    lvars win_data, list = win_data.xmp_filelist;
    XptVal list(XtN itemCount);
enddefine;

define lconstant GetSelection(win_data);
    lvars win_data;
    XpmSelectedListItems(win_data.xmp_filelist);
enddefine;

define lconstant GetSelectCount(win_data);
    lvars win_data, list = win_data.xmp_filelist;
    XptVal list(XtN selectedItemCount);
enddefine;

define lconstant SetList(string_list, win_data);
    lvars string_list, win_data;
    string_list -> XpmListItems(win_data.xmp_filelist);
enddefine;

define lconstant ReplaceItem(new_item, n, win_data);
    lvars new_item, n, win_data;
    new_item -> XpmSubscrListItems(n, win_data.xmp_filelist);
    erase();        ;;; shouldn't need this
enddefine;

define lconstant SetSelection(string_list, win_data);
    lvars string_list, win_data;
    string_list -> XpmSelectedListItems(win_data.xmp_filelist);
enddefine;

define lconstant SelectAll(win_data);
    lvars win_data, list = win_data.xmp_filelist, i, c;
    XptVal list(XmN itemCount) -> c;
    unless c == 0 then
        ;;; need to change the selection type, otherwise XmListSelectPos
        ;;; deslects previously selected items (bug?)
        ;;;
        XmMULTIPLE_SELECT -> XptVal list(XmN selectionPolicy);

        ;;; XmListSelectPos seems to toggle rather than set so clear
        ;;; any selections first
        ;;;
        XmListDeselectAllItems(list);

        fast_for i from 1 to c do
            XmListSelectPos(list, i, false);
        endfast_for;

        XmEXTENDED_SELECT -> XptVal list(XmN selectionPolicy);
    endunless;
enddefine;

define lconstant DeselectAll(win_data);
    lvars win_data, list = win_data.xmp_filelist;
    XmListDeselectAllItems(list);
    0 -> XptVal list(XtN selectedItemCount);
enddefine;

define lconstant GetCancelState(win_data);
lvars win_data, btn = win_data.xmp_cancelbtn;
    XptVal btn(XmN sensitive:XptBoolean);
enddefine;

define lconstant SetCancelState(state, win_data);
lvars state, win_data, btn = win_data.xmp_cancelbtn;

    ;;; if it's sensitive, make sure it's not blocked by the busy cursor
    ;;;
    not(state) -> XptBusyCursorFeedback(btn);
    state -> XptVal btn(XmN sensitive:XptBoolean);
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
        SetSelection(win_data);
        return;

    SELECTALL:
        SelectAll(win_data);
        return;

    DESELECTALL:
        DeselectAll(win_data);
        return;

    REPLACE:
        ReplaceItem(win_data);
        return;

    ADDITEMS:
        return;

    REMOVEITEMS:
        return;

    GETMENUBAR:
        win_data.xmp_menubar;
        return;

    GETMENUS:
        win_data.xmp_projmenu;
        win_data.xmp_selectmenu;
        win_data.xmp_editmenu;
        win_data.xmp_buildmenu;
        win_data.xmp_helpmenu;
        return;

    GETSHELL:
        win_data.xmp_shell;
        return;

    GETPARENT:
        win_data.xmp_parent;
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
    if isprocedure(win_data.xmp_on_close) then
        XptDeferApply(win_data.xmp_on_close);
        XptSetXtWakeup();
    endif;
enddefine;


define lconstant add_menu_button(parent, name, label, default, client, cb);
    lvars parent, name, label, default, client, cb, button;

    if label = nullstring then
        XtVaCreateManagedWidget(label, xmSeparatorWidget, parent,
                    #| |#) -> ;
    else
        XtVaCreateManagedWidget(name, xmPushButtonWidget, parent,
                    #|
                        XmN default, default,
                    |#) -> button;
        label -> XptVal button(XmN labelString:XmString);
        XtAddCallback(button, XmN activateCallback, cb, client);
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

    define lconstant create_menu_buttons(parent, desc, pre_cb)
                                -> menu -> button;
    lvars parent, desc, pre_cb, menu, button;

        XmCreatePulldownMenu(parent, 'menu_pane', ConsArgList(0)) -> menu;
        XtVaCreateManagedWidget(subscrv(2, desc), xmCascadeButtonGadget,
            parent, (#| XmN subMenuId, menu |#)) -> button;
        subscrv(1, desc) -> XptVal button(XmN labelString:XmString);

        if pre_cb then
            XtAddCallback(button, XmN cascadingCallback, pre_cb, menu);
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

    define lconstant list_cb(w, client, call, generic_cb);
    lvars w, client, call, generic_cb;
    l_typespec call :XmListCallbackStruct;
        generic_cb(client, XpmCoerceString(exacc call.item));
    enddefine;


    consxm_projecttool(dupnum(false, 12)) -> projtool;

    ref_widget -> projtool.xmp_parent;
    on_close -> projtool.xmp_on_close;

    lconstant bitmap_base_name = '$usepop/pop/x/ui/bitmaps/projecttool';
    xved_create_icon_window(
            xved_get_icon_filename(ref_widget, bitmap_base_name),
            false) -> projecttool_icon_window;

    lvars shell;
    XtVaCreatePopupShell('projectTool', xtTopLevelShellWidget, ref_widget,
        #|
            XtN title, sprintf(projname, 'Poplog: Project %p'),
            XtN deleteResponse, XmUNMAP,
            XtN iconWindow, projecttool_icon_window,
            XtN iconName, projname,
        |# ) ->> shell -> projtool.xmp_shell;

    lvars form;
    XtVaCreateManagedWidget('form', xmFormWidget, shell,
        #|
            XmN fractionBase, 10,
        |# ) -> form;

    lvars menubar;
    XmCreateMenuBar(form, 'menubar',
        ConsArgList(#|
                XmN orientation, XmHORIZONTAL,
                XmN topAttachment, XmATTACH_FORM,
                XmN leftAttachment, XmATTACH_FORM,
                XmN rightAttachment, XmATTACH_FORM,
            |#)) ->> menubar ->projtool.xmp_menubar;
    XtManageChild(menubar);

    lvars proj_menu, proj_widget;
    create_menu_buttons(menubar, proj_desc, menuprepare_cb)
        ->> proj_menu -> projtool.xmp_projmenu
        -> proj_widget;

    lvars select_menu, select_widget;
    create_menu_buttons(menubar, select_desc, menuprepare_cb)
        ->> select_menu -> projtool.xmp_selectmenu
        -> select_widget;

    lvars edit_menu, edit_widget;
    create_menu_buttons(menubar, edit_desc, menuprepare_cb)
        ->> edit_menu -> projtool.xmp_editmenu
        -> edit_widget;

    lvars build_menu, build_widget;
    create_menu_buttons(menubar, build_desc, menuprepare_cb)
        ->> build_menu -> projtool.xmp_buildmenu
        -> build_widget;

    lvars help_menu, help_widget;
    create_menu_buttons(menubar, help_desc, menuprepare_cb)
        ->> help_menu -> projtool.xmp_helpmenu
        -> help_widget;

    help_widget -> XptVal menubar(XmN menuHelpWidget:XptWidget);

    ;;; add the menu contents
    ;;;
    add_menu_buttons(proj_desc, proj_menu);
    add_menu_buttons(select_desc, select_menu);
    add_menu_buttons(edit_desc, edit_menu);
    add_menu_buttons(build_desc, build_menu);
    add_menu_buttons(help_desc, help_menu);

    lvars footer_form;
    XtVaCreateManagedWidget('form', xmFormWidget, form,
        #|
            XmN fractionBase, 10,
            XmN bottomAttachment, XmATTACH_FORM,
            XmN leftAttachment, XmATTACH_FORM,
            XmN rightAttachment, XmATTACH_FORM,
        |# ) -> footer_form;

    lvars cancel_button;
    XtVaCreateManagedWidget('cancelButton', xmPushButtonWidget, footer_form,
        #|
            XmN topAttachment, XmATTACH_FORM,
            XmN bottomAttachment, XmATTACH_FORM,
            XmN rightAttachment, XmATTACH_FORM,
            XmN sensitive, false,
        |#) ->> cancel_button -> projtool.xmp_cancelbtn;

    'Cancel' -> XptVal cancel_button(XmN labelString:XmString);
    false -> XptVal cancel_button(XmN resizable:XptBoolean);


    lvars footer;
    XtVaCreateManagedWidget('status', xmTextFieldWidget, footer_form,
        #|
            XmN topAttachment, XmATTACH_FORM,
            XmN bottomAttachment, XmATTACH_FORM,
            XmN leftAttachment, XmATTACH_FORM,
            XmN rightAttachment, XmATTACH_WIDGET,
            XmN rightWidget, cancel_button,
            XmN editable, false,
            XmN columns, 60,
            XmN blinkRate, 0,
            XmN cursorPositionVisible, false,
#_IF DEFV XLINK_VERSION < 1002
            XmN traversalOn, true,
#_ELSE
            XmN traversalOn, false,
#_ENDIF
            XmN marginWidth, 1,
            XmN marginHeight, 1,
        |#) ->> footer -> projtool.xmp_footer;

    ClearFooter(projtool);

    lvars list;
    XmCreateScrolledList(form, 'list',
        ConsArgList(#|
                XmN visibleItemCount, 5,
                ;;; XmN selectionPolicy, XmMULTIPLE_SELECT,
                XmN selectionPolicy, XmEXTENDED_SELECT,
                XmN automaticSelection, true,

                XmN listSizePolicy, XmCONSTANT,
                XmN scrollBarDisplayPolicy, XmSTATIC,
                XmN leftAttachment, XmATTACH_FORM,
                XmN rightAttachment, XmATTACH_FORM,
                XmN topAttachment, XmATTACH_WIDGET,
                XmN topWidget, menubar,
                XmN bottomAttachment, XmATTACH_WIDGET,
                XmN bottomWidget, footer_form,
                XmN resizable, false,
            |#)) ->> list -> projtool.xmp_filelist;

    XtManageChild(list);

    lvars cb_pdr;
    list_cb(%generic_list_cb%) -> cb_pdr;

    ;;; XtAddCallback(list, XtN multipleSelectionCallback, cb_pdr, selection_code);
    XtAddCallback(list, XtN extendedSelectionCallback, cb_pdr, selection_code);
    XtAddCallback(list, XtN defaultActionCallback, cb_pdr, action_code);

    if isprocedure(on_cancelbtn) then
        XtAddCallback(cancel_button, XtN activateCallback, on_cancelbtn, false);
    endif;

    false -> XptVal shell(XmN allowShellResize:XptBoolean);

    XtAddCallback(shell, XtN popdownCallback, NotifyClose, projtool);
enddefine;


endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 19 1995
        Changed to use CascadeButtonGadgets because HP CascadeButton widgets
        don't get the same default colours as their parent menubar
--- Robert John Duncan, Jul 11 1995
        Changed the window title
 */
