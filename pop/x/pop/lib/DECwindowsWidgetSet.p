/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.x/x/pop/lib/DECwindowsWidgetSet.p
 > Purpose:         Minimal information for the DECwindows widget-set
 > Author:          John Gibson, Feb 14 1991
 > Documentation:
 > Related Files:   LIB * XptWidgetSet
 */

#_TERMIN_IF DEF POPC_COMPILING

include xdefs
uses XptWidgetSet;

lconstant ws = XptNewWidgetSet("DECwindows");

[^^XTLIBFILES] -> ws("WidgetSetLibList");   ;;; Only correct for VMS!

[
    AttachedDBWidget
    CommandWidget
    DialogWidget
    DwtCommonWidget
    DwtHelpWidget
    FileSelectionWidget
    HelpShellWidget
    HiddenShellWidget
;;; LabelGadget
    LabelWidget
    ListBoxWidget
    MainWindowWidget
    MenuPopupWidget
    MenuWidget
    MessageWidget
    PullDownWidget
;;; PushButtonGadget
    PushButtonWidget
    ScaleWidget
    ScrollWidget
    ScrollWindowWidget
    SelectionWidget
;;; SeparatorGadget
    SeparatorWidget
    STextWidget
    TextWidget
;;; ToggleButtonGadget
    ToggleButtonWidget
    WindowWidget
    CSTextWidget
    ColorMixWidget
;;; PullDownGadget

] -> ws("WidgetSetMembers");

identfn -> ws("WidgetSetFileMapping");
