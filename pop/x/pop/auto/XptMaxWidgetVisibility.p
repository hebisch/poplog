/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptMaxWidgetVisibility.p
 > Purpose:         Ensures maximum amount of widget is visible
 > Author:          Jonathan Meyer, May 25 1991 (see revisions)
 > Documentation:   REF *XT_LIBS/XptMaxWidgetVisibility
 > Related Files:   LIB *XptCenterWidgetOn
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses fast_xt_widgetinfo;

include xpt_xscreen.ph;

define global XptMaxWidgetVisibility(widget);
    lvars widget, screen, width, height, x, y;

    l_typespec screen :XScreen;

    fast_XtScreen(XptCheckWidget(widget)) -> screen;
    XptWMShellCoords(widget) -> (x, y, width, height);
    max(0, min(x+width, exacc screen.width) - width) -> x;
    max(0, min(y+height, exacc screen.height) - height) -> y;
    x, y, false, false -> XptWMShellCoords(widget);
enddefine;

endexload_batch;
endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 27 1991
        Rewritten to just provide max visibility of the outer (WM)Shell window
--- Jonathan Meyer, Aug 17 1991
        XptGetWMShellCoords renamed XptWMShellCoords, with an updater
--- John Gibson, Aug 13 1991
        Now uses XptGetWMShellCoords to assign correct values to shell
        x, y coordinates.
--- Adrian Howard, Aug  2 1991 : Changed to use -fast_XptValue-
--- Adrian Howard, Jul 19 1991 : Fixed so it works properly for popups
--- Jonathan Meyer, Jul  6 1991
        Corrected -uses- list
 */
