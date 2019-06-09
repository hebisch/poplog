/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/xmMessageBoxWidget.p
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

XptLoadWidgetClass xmMessageBoxWidget [^^XM_EXLIBS]
    xmMessageBoxWidget  <- xmMessageBoxWidgetClass,
;

define XmIsMessageBox =
    XtIsSubclass(% xmMessageBoxWidget %)
enddefine;

XptPopLoadProcedures ''
    XmCreateMessageBox(w,x,y,z) :XptWidget,
    XmCreateMessageDialog(w,x,y,z) :XptWidget,
    XmCreateErrorDialog(w,x,y,z) :XptWidget,
    XmCreateInformationDialog(w,x,y,z) :XptWidget,
    XmCreateQuestionDialog(w,x,y,z) :XptWidget,
    XmCreateWarningDialog(w,x,y,z) :XptWidget,
    XmCreateWorkingDialog(w,x,y,z) :XptWidget,
    XmMessageBoxGetChild(x,y) :XptWidget,
;

endexload_batch;
endsection;
