/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/ved/lib/S-xvedS-menubar_xol.p
 > Purpose:         Menubar defines for OpenLook XVed
 > Author:          John Gibson, Jun  1 1993 (see revisions)
 */
compile_mode :pop11 +strict;

uses-now Xol;

section $-xved;

#_INCLUDE '$usepop/pop/x/ved/src/gui.ph'
include xpt_coretypes.ph;
include XolConstants.ph;

uses
    $-xved$-create_parent_xol,  ;;; force this come in
    xolMenuButtonGadget,
    xolStaticTextWidget,
    xolOblongButtonGadget,
    xolControlAreaWidget,
    xolMenuShellWidget,
;

/*  Vector of resource names and procedures whose elements are
 *  assigned in this file -- see xved_gui.ph
 */
lconstant menubar_gui_switch_vec = INIT_menubar_gui_switch_vec;
constant menubar_xol   = menubar_gui_switch_vec;


xolStaticTextWidget     -> wc_BLANK;
xolOblongButtonGadget   -> wc_BUTTON;
true                    -> b_HAS_STAYUP_MENUS;
XtN select              -> n_ActivateCallback;

define :macexpr p_LABEL_OF(w);
    lvars w;
    XptVal[fast] w(XtN label :XptString)
enddefine;
;;;
define :macexpr updaterof p_LABEL_OF(w);
    lvars w;
    () -> XptVal[fast] w(XtN label :XptString)
enddefine;

define :inline lconstant Set_pinnable(pinnable);
    xved_set_args(XtN pushpin, if pinnable then OL_OUT else OL_NONE endif,
                                                                    false);
enddefine;

define :macexpr p_NEW_MENUBUTTON(parent, name, pinnable) -> (button, menupane);
    lvars parent, name, pinnable, button, menupane, w, children;
    Set_pinnable(pinnable);
    fast_XtCreateManagedWidget(name, xolMenuButtonGadget, parent,
                                            xved_arg_list()) -> button;
    XptVal[fast] button(XtN menuPane:XptWidget) -> menupane;

    XtParent(menupane) -> w;
    XtSetValues(w, xved_arg_list());
    XptChildren(w) -> children;
    fast_for w in children do XtSetValues(w, xved_arg_list()) endfor;
    sys_grbg_list(children)
enddefine;

define :macexpr p_NEW_BUTTON(parent, name) -> button;
    fast_XtCreateManagedWidget(name, wc_BUTTON, parent, xved_arg_list())
                                            -> button;
enddefine;

;;; If the widget is a menubutton, return the MenuShell associated with its
;;; menuPane resource, -false- otherwise. Used to set resource values in
;;; xvedresources.p and xvedmenubar.p
define :macexpr p_HAS_MENUBAR_MENU(widget);
    lvars widget, parent;
    if fast_XtClass(widget) == xolMenuButtonGadget then
        XptVal[fast] widget(XtN menuPane:XptWidget) -> parent;
        until fast_XtClass(parent) == xolMenuShellWidget do
            fast_XtParent(parent) -> parent
        enduntil;
        parent
    else
        false
    endif
enddefine;

define :macexpr p_NEW_MENUSHELL(parent, name, pinnable) -> (mshell, mpane);
    lvars parent, name, pinnable, mshell, mpane;
    Set_pinnable(pinnable);
    fast_XtCreatePopupShell(name, xolMenuShellWidget, parent, xved_arg_list())
                                                        -> mshell;
    XptVal[fast] mshell(XtN menuPane:XptWidget) -> mpane
enddefine;

define :macexpr p_NEW_MENUBAR(parent, window) -> menubar;
    lvars parent, window, menubar, sb;
    ;;; some args already set
    xved_set_args([
                ^XtN xResizable     ^true
                ^XtN xAttachRight   ^true
                ^XtN xVaryOffset    ^false
            ], false);
    XtCreateWidget('menubar', xolControlAreaWidget, parent, xved_arg_list())
                            -> menubar
enddefine;

define :macexpr p_POPUP_MENU(popupmenu, window, button);
    lvars popupmenu, window, button;
    true -> xved_is_raised_window(popupmenu); ;;; may be pinned, so raise it
    OlMenuPost(popupmenu);
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Oct  9 1997
        Added p_NEW_BUTTON for compatibility with Motif
 */
