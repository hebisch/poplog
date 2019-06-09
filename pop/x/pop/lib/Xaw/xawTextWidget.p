/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xaw/xawTextWidget.p
 > Purpose:         Athena widgetclass and associated procedures
 > Author:          John Gibson, Apr 17 1993
 > Documentation:   REF * ATHENA
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include xpt_coretypes.ph;
include XawConstants.ph;

uses xawAsciiSinkObject, xawAsciiSrcObject;

XptLoadWidgetClass xawTextWidget [^^XAW_EXLIBS]
    xawTextWidget   <- textWidgetClass,
;

define XawIsTextWidget =
    XtIsSubclass(% xawTextWidget %)
enddefine;

shadowclass XpawTextBlockPtr {:XpawTextBlock};

XptPopLoadProcedures ''
    XawTextDisplay(x) :void,
    XawTextEnableRedisplay(x) :void,
    XawTextDisableRedisplay(x) :void,
    XawTextSetSelectionArray(x,y) :void,
    XawTextGetSelectionPos(x,y,z) :void,
    XawTextSetSource(x,y,z) :void,
    XawTextReplace(w,x,y,z) :int,
    XawTextTopPosition(x) :ulong,
    XawTextSetInsertionPoint(x,y) :void,
    XawTextGetInsertionPoint(x) :ulong,
    XawTextUnsetSelection(x) :void,
    XawTextSetSelection(x,y,z) :void,
    XawTextInvalidate(x,y,z) :void,
    XawTextGetSource(x) :XptWidget,
    XawTextSearch(x,y,z) :long,
;

endexload_batch;
endsection;
