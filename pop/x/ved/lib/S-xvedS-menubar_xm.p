/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/ved/lib/S-xvedS-menubar_xm.p
 > Purpose:         Menubar defines for Motif XVed
 > Author:          John Gibson, Jun  1 1993 (see revisions)
 */
compile_mode :pop11 +strict;

uses-now Xm;

section $-xved;

#_INCLUDE '$usepop/pop/x/ved/src/gui.ph'
include xpt_coretypes.ph;
include xved_constants.ph;
include XmConstants.ph;

uses
    $-xved$-create_parent_xm,   ;;; force this come in
    xmMainWindowWidget,
    xmRowColumnWidget,
    xmCascadeButtonGadget,
    xmSeparatorGadget,
    xmPushButtonGadget,
;

/*  Vector of resource names and procedures whose elements are
 *  assigned in this file -- see xved_gui.ph
 */
lconstant menubar_gui_switch_vec = INIT_menubar_gui_switch_vec;
constant menubar_xm   = menubar_gui_switch_vec;


xmSeparatorGadget       -> wc_BLANK;
xmPushButtonGadget      -> wc_BUTTON;
false                   -> b_HAS_STAYUP_MENUS;
XtN activateCallback    -> n_ActivateCallback;

define :macexpr p_LABEL_OF(w);
    lvars w;
    XptVal[fast] w(XtN labelString :XmString#XpmCoerceString)
enddefine;
;;;
define :macexpr updaterof p_LABEL_OF(w);
    lvars w;
    () -> XptVal[fast] w(XtN labelString :XmString#XpmCoerceString)
enddefine;

define :inline lconstant Set_pinnable(pinnable);
;;; TearOff menus don't seem to work (they fall over when torn off)
;;; xved_set_args(XmN tearOffModel, if pinnable then XmTEAR_OFF_ENABLED
;;;                                 else XmTEAR_OFF_DISABLED
;;;                                 endif, false);
enddefine;

    ;;; Make a blank pulldown menu
define :macexpr p_NEW_MENUBUTTON(parent, name, pinnable) -> (button, menu);
    lvars parent, name, pinnable, button, menu;
    XmCreateCascadeButtonGadget(parent, 'menu_button', xved_arg_list()) -> button;
    Set_pinnable(pinnable);
    XmCreatePulldownMenu(parent, 'menu_pane', xved_arg_list()) -> menu;
    lvars label = XmStringCreateLocalized(name);
    (label, menu) -> XptVal[fast] button(XtN labelString:exptr, XtN subMenuId:XptWidget);
    XmStringFree(label);
    if name = XVMB_HELP_LABEL then
        button -> XptVal[fast] parent(XtN menuHelpWidget:XptWidget);
    endif;
    fast_XtManageChild(button);
enddefine;

    ;;; Create a push button
define :macexpr p_NEW_BUTTON(parent, name) -> button;
    XmCreatePushButton(parent, 'button', xved_arg_list()) -> button;
    lvars label = XmStringCreateLocalized(name);
    label -> XptVal[fast] button(XtN labelString:exptr);
    XmStringFree(label);
    fast_XtManageChild(button);
enddefine;

;;; If the given widget has a menu pane, return it. Return -false- otherwise.
define :macexpr p_HAS_MENUBAR_MENU(widget);
    lvars widget, class = fast_XtClass(widget);
    (class == xmCascadeButtonGadget or class == xmCascadeButtonWidget)
    and XptVal[fast] widget(XtN subMenuId:XptWidget)
enddefine;

define :macexpr p_NEW_MENUSHELL(parent, name, pinnable) /* -> (shell, pane) */;
    lvars parent, name, pinnable;
    Set_pinnable(pinnable);
    ;;; Motif doesn't need both MenuPanes and MenuShells, so use dup
    dup(XmCreatePopupMenu(parent, name, xved_arg_list()))
enddefine;

define :macexpr p_NEW_MENUBAR(parent, window) /* -> menubar */;
    lvars parent, window;
    ;;; some args already set
    XmCreateMenuBar(parent, 'menubar', xved_arg_list())
enddefine;

define :macexpr p_POPUP_MENU(popupmenu, window, button);
    lvars popupmenu, window, button, (, x, y) = xved_x_query_pointer(false);
    '<Btn' sys_>< button sys_>< 'Down>:'
                    -> XptVal popupmenu(XmN menuPost :XptString);
    x, y, false, false  -> XptWidgetCoords(popupmenu);
    XtManageChild(popupmenu);
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Oct  9 1997
        Fixed p_NEW_MENUBUTTON and added p_NEW_BUTTON so that button labels
        can be Unicode
--- Robert John Duncan, Jul 19 1995
        Changed to use CascadeButtonGadgets because HP CascadeButton widgets
        don't get the same default colours as their parent menubar
--- John Gibson, Feb 28 1994
        Set XmN tearOffModel for menus from pinnable arg -- but unfortunately,
        they don't seem to work.
--- John Gibson, Dec 17 1993
        Changed p_POPUP_MENU to use new definition of xved_x_query_pointer
        and to assign button event to XmN menuPost
 */
