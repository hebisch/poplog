/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/ved/src/gui.ph
 > Purpose:         Defines for Motif/OpenLook XVed
 > Author:          John Gibson, Jun  1 1993 (see revisions)
 */

#_TERMIN_IF DEF GUI_INCLUDED

section;

include define_macexpr.ph;

GEN_VECSUB_MACROS scrollbar_gui_switch_vec
#_< [
    ;;; scrollbar widgetclass
    wc_SCROLLBAR

    ;;; lists of scrollbar args
    l_SCROLLBAR_ARGS
    l_HSCROLLBAR_ARGS

    ;;; resource names
    n_ScrollbarMoved
    n_ScrollbarDragged

    ;;; procedures
    p_SET_SCROLLBAR
    p_SCROLLBAR_VALUE
    p_HAS_SCROLLBAR_MENU
    p_SCROLL_CB_VERIFY

] >_#;

GEN_VECSUB_MACROS menubar_gui_switch_vec
#_< [
    ;;; widgetclasses
    wc_BLANK
    wc_BUTTON

    ;;; true if has stayup menus
    b_HAS_STAYUP_MENUS

    ;;; resource names
    n_ActivateCallback

    ;;; procedures
    p_LABEL_OF
    p_NEW_MENUBUTTON
    p_NEW_BUTTON
    p_HAS_MENUBAR_MENU
    p_NEW_MENUSHELL
    p_NEW_MENUBAR
    p_POPUP_MENU

] >_#;

iconstant GUI_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Oct  9 1997
        Added p_NEW_BUTTON
--- John Gibson, Dec 23 1993
        Improved scrollbar interface
 */
