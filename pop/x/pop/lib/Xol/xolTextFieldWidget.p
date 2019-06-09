/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/xolTextFieldWidget.p
 > Purpose:         Define Xol widgetclass
 > Author:          John Gibson, Mar 31 1993
 > Documentation:   HELP * OPENLOOK
 > Related Files:   x/pop/lib/Xol/xol*Widget.p
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses Xolgeneral, XolManager;

XptLoadWidgetClass xolTextFieldWidget [^^XOL_EXLIBS]
    xolTextFieldWidget <- textFieldWidgetClass
;

XptPopLoadProcedures '' [^^XOL_EXLIBS]
    OlTextFieldCopyString(x,y) :int,
    OlTextFieldGetString(x,y) :exptr.exacc_ntstring,
;

endexload_batch;
endsection;
