/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xpt_cursorplane.p
 > Purpose:         Manages 'cursor planes' for widgets
 > Author:          Jonathan Meyer, Aug 15 1991 (see revisions)
 > Documentation:   REF *XPT_CURSORPLANE
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

include xpt_xtypes.ph;
uses xt_widget;
uses xt_widgetclass;
uses xt_widgetinfo;
uses xt_callback;
uses xt_display;

lconstant macro (
    WINDOW_SIZE = 1e4,      /* cursor plane is 10000 x 10000 pixels */

    ;;; perwidget slots
    PW_DISPLAY = 1, PW_WIN = 2, PW_CURSOR = 3, PW_ENABLED = 4,
  );

/* The following Xlib procedures are used: */
XptLoadProcedures xpt_cursorplane
    lvars
        XDefineCursor,
        XUndefineCursor,
        XCreateWindow,
        XMapRaised,
        XUnmapWindow,
    ;

/* Misc functions */

define lconstant Check_cursor(cursor) -> cursor;
    lvars cursor;
    unless cursor == false or cursor.isinteger
            or not(is_null_external_ptr(cursor)) then
        mishap(cursor,1, 'INVALID CURSOR ID');
    endunless;
enddefine;


/* PERWIDGET info */

define lconstant Destroy_cb(w, prop, call);
    lvars w, prop, call;
    /* remove entry from perwidget assoc */
    false -> prop(w);
enddefine;

define lconstant active_perwidget(w, prop);
    lvars w, prop, win, dpy = XtDisplay(w), pw, pd;

    unless fast_XtIsWidget(w) then
        mishap(w,1,'WIDGET NEEDED');
    endunless;

    unless fast_XtIsRealized(w) then
        mishap(w,1,'REALIZED WIDGET NEEDED');
    endunless;

    ;;; create a very large transparent InputOnly window
    exacc (12):XptXID raw_XCreateWindow(dpy, fast_XtWindow(w),
            0,0, WINDOW_SIZE, WINDOW_SIZE, 0,0, 2, 0, 0,0) -> win;

    ;;; build perwidget info
    {%dpy, win, false, false %} -> pw;

    ;;; destroy action
    XtAddCallback(w, XtN destroyCallback, Destroy_cb, prop);

    pw ->> prop(w);
enddefine;

lconstant
    perwidget = writeable newanyproperty([], 10,1,10, false, false,
                        "tmparg", false, active_perwidget);

/* checks the widgets cursor is up to date */
define lconstant Reset_cursor(w, pw, cursor, enabled);
    lvars w, dpy, pw, cursor, win, enabled;
    returnif(is_null_external_ptr(w));
    fast_subscrv(PW_DISPLAY, pw) -> dpy;
    fast_subscrv(PW_WIN, pw) -> win;
    if cursor and enabled then
        exacc [fast] (3) raw_XDefineCursor(dpy, win, cursor);
        exacc [fast] (2) raw_XMapRaised(dpy, win);
    else
        exacc [fast] (2) raw_XUndefineCursor(dpy, win);
        exacc [fast] (2) raw_XUnmapWindow(dpy, win);
    endif;
enddefine;

/* PUBLIC FUNCTIONS */

define XptCursorPlaneOn(w);
    lvars w;
    fast_subscrv(PW_ENABLED, perwidget(w))
enddefine;

define updaterof XptCursorPlaneOn(enabled, w);
    lvars enabled, w, pw;
    perwidget(w) -> pw;
    enabled -> fast_subscrv(PW_ENABLED, pw);
    Reset_cursor(w, pw, fast_subscrv(PW_CURSOR, pw), enabled);
enddefine;

define XptNewCursorPlaneCursor(w, new_cursor) -> old_cursor;
    lvars w, pw, new_cursor, old_cursor;
    perwidget(w) -> pw;
    fast_subscrv(PW_CURSOR, pw) -> old_cursor;
    if new_cursor /== undef then
        ;;; update new_cursor
        Check_cursor(new_cursor) -> fast_subscrv(PW_CURSOR, pw);
        Reset_cursor(w, pw, new_cursor, fast_subscrv(PW_ENABLED, pw));
    endif;
enddefine;

constant xpt_cursorplane = true; ;;; for uses

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, May  9 1995
        Added missing uses declarations
--- Simon Nichols, Dec  3 1991
        Reduced WINDOW_SIZE to 1e4 for the benefit of HP-UX 8.0 X server.
--- Jonathan Meyer, Sep  2 1991 Added uses xpt_xtypes;
 */
