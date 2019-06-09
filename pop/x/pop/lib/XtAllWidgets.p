/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/XtAllWidgets.p
 > Purpose:         Load all Xt widgets
 > Author:          John Gibson, Apr 25 1993
 > Documentation:
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses
    xtApplicationShellWidget,
    xtCompositeWidget,
    xtConstraintWidget,
    xtCoreWidget,
    xtObject,
    xtOverrideShellWidget,
    xtRectObj,
    xtShellWidget,
    xtTopLevelShellWidget,
    xtTransientShellWidget,
    xtVendorShellWidget,
    xtWmShellWidget,
;

constant XtAllWidgets = true;

endexload_batch;
endsection;
