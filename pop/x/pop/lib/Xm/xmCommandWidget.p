/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/xmCommandWidget.p
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

XptLoadWidgetClass xmCommandWidget [^^XM_EXLIBS]
    xmCommandWidget <- xmCommandWidgetClass,
;

XptPopLoadProcedures ''
    XmCreateCommand(w,x,y,z) :XptWidget,
    XmCommandGetChild(x,y) :XptWidget,
    XmCommandSetValue(x,y) :void,
    XmCommandAppendValue(x,y) :void,
    XmCommandError(x,y) :void,
;

endexload_batch;
endsection;
