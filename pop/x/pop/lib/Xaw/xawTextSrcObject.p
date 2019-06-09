/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xaw/xawTextSrcObject.p
 > Purpose:         Athena widgetclass and associated procedures
 > Author:          John Gibson, Apr 17 1993
 > Documentation:   REF * ATHENA
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include xpt_coretypes.ph;
include XawConstants.ph;

XptLoadWidgetClass xawTextSrcObject [^^XAW_EXLIBS]
    xawTextSrcObject    <- textSrcObjectClass,
;

define XawIsTextSrcObject =
    XtIsSubclass(% xawTextSrcObject %)
enddefine;

XptPopLoadProcedures ''
    XawTextSourceRead(w,x,y,z) :XawTextPosition,
    XawTextSourceReplace(w,x,y,z) :int,
    XawTextSourceScan(u,v,w,x,y,z) :XawTextPosition,
    XawTextSourceSearch(w,x,y,z) :XawTextPosition,
    XawTextSourceConvertSelection(t,u,v,w,x,y,z) :XptLongBoolean,
    XawTextSourceSetSelection(w,x,y,z) :void,
;

endexload_batch;
endsection;
