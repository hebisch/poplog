/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/xolTextEditWidget.p
 > Purpose:         Define Xol widgetclass
 > Author:          John Gibson, Mar 31 1993
 > Documentation:   HELP * OPENLOOK
 > Related Files:   x/pop/lib/Xol/xol*Widget.p
 */
compile_mode :pop11 +strict;

include xpt_coretypes.ph;

section;
exload_batch;

uses Xolgeneral, XolDynamic, Xolbuffer;

XptLoadWidgetClass xolTextEditWidget [^^XOL_EXLIBS]
    xolTextEditWidget <- textEditWidgetClass
;

XptPopLoadProcedures '' [^^XOL_EXLIBS]
    OlTextEditClearBuffer(x) :XptBoolean,
    OlTextEditCopyBuffer(x,y) :XptBoolean,
    OlTextEditCopySelection(x,y) :XptBoolean,
    OlTextEditReadSubString(w,x,y,z) :XptBoolean,
    OlTextEditGetLastPosition(x,y) :XptBoolean,
    OlTextEditGetCursorPosition(w,x,y,z) :XptBoolean,
    OlTextEditInsert(x,y,z) :XptBoolean,
    OlTextEditPaste(x) :XptBoolean,
    OlTextEditSetCursorPosition(w,x,y,z) :XptBoolean,
    OlTextEditRedraw(x) :XptBoolean,
    OlTextEditUpdate(x,y) :XptBoolean,
    OlTextEditTextBuffer(x) :exptr,
;

endexload_batch;
endsection;
