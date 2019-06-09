/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xaw/xawAsciiSrcObject.p
 > Purpose:         Athena widgetclass and associated procedures
 > Author:          John Gibson, Apr 17 1993
 > Documentation:   REF * ATHENA
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include xpt_coretypes.ph;

uses xawTextSrcObject;

XptLoadWidgetClass xawAsciiSrcObject [^^XAW_EXLIBS]
    xawAsciiSrcObject   <- asciiSrcObjectClass,
;

define XawIsAsciiSrcObject =
    XtIsSubclass(% xawAsciiSrcObject %)
enddefine;

XptPopLoadProcedures ''
    XawAsciiSourceFreeString(x) :void,
    XawAsciiSave(x) :XptBoolean,
    XawAsciiSaveAsFile(x,y) :XptBoolean,
    XawAsciiSourceChanged(x) :XptBoolean,
;

endexload_batch;
endsection;
