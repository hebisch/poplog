/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/xmFileSelectionBoxWidget.p
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

XptLoadWidgetClass xmFileSelectionBoxWidget [^^XM_EXLIBS]
    xmFileSelectionBoxWidget    <- xmFileSelectionBoxWidgetClass,
;

define XmIsFileSelectionBox =
    XtIsSubclass(% xmFileSelectionBoxWidget %)
enddefine;

XptPopLoadProcedures ''
    XmFileSelectionBoxGetChild(x,y) :XptWidget,
    XmFileSelectionDoSearch(x,y) :void,
    XmCreateFileSelectionBox(w,x,y,z) :XptWidget,
    XmCreateFileSelectionDialog(w,x,y,z) :XptWidget,
;

endexload_batch;
endsection;
