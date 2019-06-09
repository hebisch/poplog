/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xaw/xawListWidget.p
 > Purpose:         Athena widgetclass and associated procedures
 > Author:          John Gibson, Apr 17 1993
 > Documentation:   REF * ATHENA
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses xawSimpleWidget;

XptLoadWidgetClass xawListWidget [^^XAW_EXLIBS]
    xawListWidget   <- listWidgetClass,
;

define XawIsListWidget =
    XtIsSubclass(% xawListWidget %)
enddefine;

XptPopLoadProcedures ''
    XawListChange(v,w,x,y,z) :void,
    XawListHighlight(x,y) :void,
    XawListUnhighlight(x) :void,
    XawListShowCurrent(x) :exptr,   ;;; an XawListReturnStruct
;

endexload_batch;
endsection;
