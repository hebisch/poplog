/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xaw/xawTextSinkObject.p
 > Purpose:         Athena widgetclass and associated procedures
 > Author:          John Gibson, Apr 17 1993
 > Documentation:   REF * ATHENA
 */
compile_mode :pop11 +strict;

section;
exload_batch;

XptLoadWidgetClass xawTextSinkObject [^^XAW_EXLIBS]
    xawTextSinkObject   <- textSinkObjectClass,
;

define XawIsTextSinkObject =
    XtIsSubclass(% xawTextSinkObject %)
enddefine;

XptPopLoadProcedures ''
    XawTextSinkClearToBackground(v,w,x,y,z) :void,
    XawTextSinkDisplayText(u,v,w,x,y,z) :void,
    XawTextSinkFindDistance(t,u,v,w,x,y,z) :void,
    XawTextSinkFindPosition(s,t,u,v,w,x,y,z) :void,
    XawTextSinkGetCursorBounds(x,y) :void,
    XawTextSinkInsertCursor(w,x,y,z) :void,
    XawTextSinkMaxHeight(x,y) :int,
    XawTextSinkMaxLines(x,y) :int,
    XawTextSinkResolve(v,w,x,y,z) :void,
    XawTextSinkSetTabs(x,y,z) :void,
;

endexload_batch;
endsection;
