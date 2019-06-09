/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/xmRowColumnWidget.p
 > Purpose:         Motif widgetclass
 > Author:          John Gibson, Apr 14 1993
 > Documentation:   HELP * MOTIF
 > Related Files:   Xm/xm*Widget.p etc
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include xpt_coretypes.ph;

uses Xmgeneral;

XptLoadWidgetClass xmRowColumnWidget [^^XM_EXLIBS]
    xmRowColumnWidget   <- xmRowColumnWidgetClass,
;

define XmIsRowColumn =
    XtIsSubclass(% xmRowColumnWidget %)
enddefine;

XptPopLoadProcedures ''

    ;;; SimpleMenu declarations and definitions
    XmCreateSimpleMenuBar(w,x,y,z) :XptWidget,
    XmCreateSimplePopupMenu(w,x,y,z) :XptWidget,
    XmCreateSimplePulldownMenu(w,x,y,z) :XptWidget,
    XmCreateSimpleOptionMenu(w,x,y,z) :XptWidget,
    XmCreateSimpleRadioBox(w,x,y,z) :XptWidget,
    XmCreateSimpleCheckBox(w,x,y,z) :XptWidget,
    XmVaCreateSimpleMenuBar(...) :XptWidget,
    XmVaCreateSimplePopupMenu(...) :XptWidget,
    XmVaCreateSimplePulldownMenu(...) :XptWidget,
    XmVaCreateSimpleOptionMenu(...) :XptWidget,
    XmVaCreateSimpleRadioBox(...) :XptWidget,
    XmVaCreateSimpleCheckBox(...) :XptWidget,

    XmCreateRadioBox(w,x,y,z) :XptWidget,
    XmCreateRowColumn(w,x,y,z) :XptWidget,
    XmCreateWorkArea(w,x,y,z) :XptWidget,
    XmCreatePopupMenu(w,x,y,z) :XptWidget,
    XmCreatePulldownMenu(w,x,y,z) :XptWidget,
    XmCreateOptionMenu(w,x,y,z) :XptWidget,
    XmCreateMenuBar(w,x,y,z) :XptWidget,
    XmOptionLabelGadget(x) :XptWidget,
    XmOptionButtonGadget(x) :XptWidget,
    XmGetPostedFromWidget(x) :XptWidget,
    XmMenuPosition(x,y) :void,
;

endexload_batch;
endsection;
