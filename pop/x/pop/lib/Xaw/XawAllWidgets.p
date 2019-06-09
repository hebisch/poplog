/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xaw/XawAllWidgets.p
 > Purpose:         Load all Xaw widgets
 > Author:          John Gibson, Apr 25 1993
 > Documentation:   REF * ATHENA
 */
compile_mode :pop11 +strict;

section;
exload_batch

uses
    xawAsciiSinkObject,
    xawAsciiSrcObject,
    xawAsciiTextWidget,
    xawBoxWidget,
    xawClockWidget,
    xawCommandWidget,
    xawDialogWidget,
    xawFormWidget,
    xawGripWidget,
    xawLabelWidget,
    xawListWidget,
    xawLogoWidget,
    xawMailboxWidget,
    xawMenuButtonWidget,
    xawPanedWidget,
    xawScrollbarWidget,
    xawSimpleMenuWidget,
    xawSimpleWidget,
    xawSmeBSBObject,
    xawSmeLineObject,
    xawSmeObject,
    xawStripChartWidget,
    xawTextSinkObject,
    xawTextSrcObject,
    xawTextWidget,
    xawToggleWidget,
    xawViewportWidget,
;

constant XawAllWidgets = true;

endexload_batch;
endsection;
