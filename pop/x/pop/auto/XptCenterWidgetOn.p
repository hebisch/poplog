/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptCenterWidgetOn.p
 > Purpose:         Places center point of widget over reference location
 > Author:          Jonathan Meyer, May 25 1991 (see revisions)
 > Documentation:   REF *XT_LIBS/XptCenterWidgetOn
 > Related Files:   LIB *XptMaxWidgetVisibility
 */
compile_mode :pop11 +strict;

section;

include xpt_coretypes.ph;

exload_batch;
uses fast_xt_widgetinfo;
uses fast_xt_widgetclass;
uses fast_xt_util;
endexload_batch;

define global XptCenterWidgetOn(widget, reference);
    lvars widget, reference, screen, x, y, sx, sy;
    lconstant xpos = writeable initshortvec(1),
                ypos = writeable initshortvec(1);

    l_typespec screen {
        ext_data: exptr,
        display: exptr,
        root: exptr,
        width: int,
        height: int,
    };

    define lconstant half_dim(widget);
        lvars widget, (w, h) =
            XptVal widget(XtN width:XptDimension, XtN height:XptDimension);
        w fi_div 2, h fi_div 2
    enddefine;

    if reference.isinteger then
        (widget, reference) -> (widget,x,y);
    endif;

    if reference == "screen" then
        fast_XtScreen(XptCheckWidget(widget)) -> screen;
        exacc screen.width fi_div 2 -> x;
        exacc screen.height fi_div 2 -> y;
    elseunless reference.isinteger then
        XptCheckWidget(reference) -> reference;
        half_dim(reference) -> (x, y);
        fast_XtTranslateCoords(reference, x,y, xpos, ypos);
        xpos(1) -> x; ypos(1) -> y;
    endif;

    half_dim(widget) -> (sx, sy);
    x fi_- sx -> x; y fi_- sy -> y;

    fast_XtTranslateCoords(widget, 0,0, xpos, ypos);
    unless fast_XtIsShell(widget) then
        ;;; translate from childs x,y to shells x,y coord
        xpos(1) -> sx; ypos(1) -> sy;
        until fast_XtIsShell(fast_XtParent(widget) ->> widget) do; enduntil;
        fast_XtTranslateCoords(widget, 0,0, xpos, ypos);
        x fi_- (sx fi_- xpos(1)) -> x;
        y fi_- (sy fi_- ypos(1)) -> y;
    endunless;
    ;;; x, y, width, height
    x /== xpos(1) and x, y /== ypos(1) and y,
    false, false -> XptWidgetCoords(widget);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 10 1992 Changed to use XptVal
--- Jonathan Meyer, Sep  2 1991 Changed to use XptWidgetCoords
--- Adrian Howard, Jul 19 1991 : Fixed so it works correctly with popups
 */
