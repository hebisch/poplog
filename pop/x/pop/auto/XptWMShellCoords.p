/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptWMShellCoords.p
 > Purpose:         Set/Returns x/y coordinates of windowmanager wrapper
 > Author:          Jonathan Meyer, Jul 31 1991 (see revisions)
 > Documentation:   REF XT_LIBS/XptWMShellCoords
 > Related Files:   LIB XptShellOfObject
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include xpt_xtypes.ph;
uses fast_xt_display;
uses fast_xt_widgetinfo;

XptLoadProcedures XptWMShellCoords
lvars
    XQueryTree(dpy,win,root,parent,children,nchildren),
    XGetGeometry(dpy,win,root,x,y,width,height,bwidth,depth),
    XFree(ptr),
;

lvars
    last_x_correction = 0,
    last_y_correction = 0,
;

define XptWMShellCoords(w) /* -> (x, y, width, height) */;
    lvars w, dpy, win, bw, pwin;
    lconstant
        dummy       = EXPTRINITSTR(:int),
        root        = EXPTRINITSTR(:XptXID),
        parent      = EXPTRINITSTR(:XptXID),
        children    = EXPTRINITSTR(:exptr),
        x_ptr       = EXPTRINITSTR(:int),
        y_ptr       = EXPTRINITSTR(:int),
        width_ptr   = EXPTRINITSTR(:uint),
        height_ptr  = EXPTRINITSTR(:uint),
        bw_ptr      = EXPTRINITSTR(:uint),
    ;
    XptShellOfObject(w) -> w;
    fast_XtWindow(w) -> win;

    if win then
        fast_XtDisplay(w) -> dpy;

        repeat
            exacc raw_XQueryTree(dpy, win, root, parent, children, dummy);
            exacc raw_XFree(exacc[nc] :exptr children);
            exacc :XptXID parent -> pwin;
            quitif(pwin = exacc :XptXID root);
            pwin -> win
        endrepeat;

        XptSyncDisplay(dpy);

        ;;; win is now a child of root
        exacc raw_XGetGeometry(dpy, win, root,
                    x_ptr, y_ptr, width_ptr, height_ptr, bw_ptr, dummy);
        exacc :uint bw_ptr fi_<< 1 -> bw;
        exacc :int x_ptr,
        exacc :int y_ptr,
        exacc :uint width_ptr fi_+ bw,
        exacc :uint height_ptr fi_+ bw
    else
        ;;; just return the shell coords
        XptWidgetCoords(w)
    endif
enddefine;


    /*  The mucking around with last_x_correction and last_y_correction
        in the updater below is to get round the behaviour of the
        twm window manager, which interprets assignments to XtNx and XtNy
        for a shell widget as being the position of its wrapper window
        (meaning that the values that come out afterwards are not the values
        assigned in, making nonsense of the semantics of these fields).

        So we assign the values in, then get them again afterwards, and
        remember the difference for subsequent operations (in mwm and olwm
        the corrections will always be 0).

        Note that twm's behaviour gives rise to the following bug:
        assigning x, y values which (according to its interpretation) will
        leave the window position unchanged means that no ConfigureNotify
        event results; this causes the shell RootGeometryManager to time out,
        leaving the shell coordinates wrong. While the code below doesn't
        attempt to cope with this directly, using the corrections will avoid
        the bug -- that is, it can only happen the first time before the
        x and y corrections have been set up.
    */
define updaterof XptWMShellCoords(x, y, width, height, w);
    lvars   x, y, width, height, w, wx, wy, wwidth, wheight,
            sx, sy, swidth, sheight, req_x, req_y, gstr;

    XptShellOfObject(w) -> w;

    unless fast_XtIsRealized(w) then
        ;;; integrate with XtN geometry
        XptParseGeometry(XptVal[fast] w(XtN geometry:XptString))
                            -> (sx, sy, swidth, sheight);
        unless x then sx -> x endunless;
        unless y then sy -> y endunless;
        unless width then swidth -> width endunless;
        unless height then sheight -> height endunless;
        (x, y, width, height) -> XptParseGeometry()
                                -> XptVal[fast] w(XtN geometry:XptString);
        return

    elseif XptVal[fast] w(XtN overrideRedirect:XptBoolean) then
        (x, y, width, height) -> XptWidgetCoords(w);
        return
    endunless;

    XptWMShellCoords(w) -> (sx, sy, swidth, sheight);
    XptWidgetCoords(w)  -> (wx, wy, wwidth, wheight);

    ;;; translate to required shell coords

    if x.isinteger then
        x fi_+ wx fi_- sx ->> x -> req_x;
        if x == wx then
            false -> x
        elseif (x fi_+ last_x_correction ->> x) == wx then
            x fi_- sign(last_x_correction) -> x
        endif
    else
        false -> x
    endif;
    if y.isinteger then
        y fi_+ wy fi_- sy ->> y -> req_y;
        if y == wy then
            false -> y
        elseif (y fi_+ last_y_correction ->> y) == wy then
            y fi_- sign(last_y_correction) -> y
        endif
    else
        false -> y
    endif;
    if width.isinteger then
        width fi_+ wwidth fi_- swidth -> width
    endif;
    if height.isinteger then
        height fi_+ wheight fi_- sheight -> height
    endif;

    (x,y,width,height) -> XptWidgetCoords(w);
    returnunless(x or y);

    ;;; ensure change has taken effect before interrogating x, y values again
    XptSyncDisplay(fast_XtDisplay(w));

    XptWidgetCoords(w) -> (wx, wy, , );
    if x then
        x fi_- wx -> last_x_correction;
        if wx == req_x then false else req_x fi_+ last_x_correction endif
    else
        false
    endif -> x;
    if y then
        y fi_- wy -> last_y_correction;
        if wy == req_y then false else req_y fi_+ last_y_correction endif
    else
        false
    endif -> y;
    if x or y then
        ;;; make the correction
        (x, y, false, false) -> XptWidgetCoords(w)
    endif
enddefine;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  1 1995
        Replaced use of int*vecs with EXPTRINITSTRs for correct types
--- John Gibson, Mar 21 1994
        Changed to use new XptSyncDisplay
--- John Gibson, Feb  8 1994
        Added syncing in XptWMShellCoords
--- John Gibson, Dec 15 1993
        Changed updater to integrate args into XtN geometry spec if shell
        is not realised
--- John Gibson, Sep  7 1992
        Changed to use XptVal
--- John Gibson, Sep 27 1991
        Rewrote the updater so it works properly with all WMs.
--- John Gibson, Sep 20 1991
        Rewrote XptWMShellCoords to cope with multi-level WM wrapper
        windows
--- Jonathan Meyer, Aug 17 1991
        Renamed XptWMShellCoords and added updater
--- Jonathan Meyer, Aug  1 1991
        Made it work with unrealized widgets.
--- Jonathan Meyer, Jul 31 1991 Renamed Wm -> WM
 */
