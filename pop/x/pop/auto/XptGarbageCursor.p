/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptGarbageCursor.p
 > Purpose:         Gets the standard Garbage and Busy Cursors
 > Author:          Jonathan Meyer, Sep  1 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include x_cursorfont;
include xpt_xcolor;
include xpt_xscreen;

uses xt_resource;
uses xt_widgetinfo;

XptLoadProcedures XptGarbageCursor
    lvars XCreateFontCursor, XQueryColors, XRecolorCursor,
            XDefaultScreenOfDisplay
;

lconstant
    macro (BUSY_CURSOR = "fast_front", GC_CURSOR = "fast_back"),
;

vars procedure XptBusyCursorChangeTrap = identfn;

define lconstant RecolorCursor(dpy, cursor, fg, bg);
    lvars dpy, cursor, fg, bg, cmap;
    l_typespec colors :XColor[2];
    lconstant colors = EXPTRINITSTR(:colors);

    fg -> exacc (exacc colors[1]).pixel;
    bg -> exacc (exacc colors[2]).pixel;
    exacc (exacc (1):exptr.:XScreen raw_XDefaultScreenOfDisplay(dpy)).cmap
                                                                -> cmap;
    exacc (4) raw_XQueryColors(dpy, cmap, colors, 2);
    exacc (4) raw_XRecolorCursor(dpy, cursor,
                                        exacc colors[1], exacc colors[2]);
enddefine;


/* procedure for searching perdisplay lists - optimized to be called
   multiple times with the same display
*/
define lconstant Search_list(key, list, new_p) -> (value, list);
    lvars key, list, pair, akey, value, procedure new_p;
    ;;; check front of list first
    fast_destpair(fast_front(list)) -> (akey, value);
    returnif(akey == key);
    ;;; search list
    fast_for pair on list do
        fast_destpair(fast_front(pair)) -> (akey, value);
        quitif(akey == key);
    endfast_for;
    if akey == key then
        ;;; swap position so it is at the front of the list
        fast_front(list),fast_front(pair)->(fast_front(pair),fast_front(list));
    else
        ;;; create new entry
        new_p(key) -> value;
        conspair(conspair(key, value), list) -> list;
    endif;
enddefine;

;;; OpenLook actually has a "busy" cursor - so use that if its there
weak constant procedure GetOlBusyCursor;

define lconstant Create_cursors(dpy) -> cursors;
    lvars dpy, w, busy_cursor, garbage_cursor, cursors, nres;

    shadowclass lconstant XptResourceList [nc, prefix nc_];

    l_typespec AppData
        {   gc_cursor   :XptCursor,
            gc_fg       :XptPixel,
            gc_bg       :XptPixel,
            by_cursor   :XptCursor,
            by_fg       :XptPixel,
            by_bg       :XptPixel
        };

    define :inline lconstant OFFSET(field=item);
        FIELDOFFSET(:AppData,field)
    enddefine;

    lconstant
        appl_resources = nonwriteable {%
            (XtN GarbageCursor, XtC GarbageCursor, XtR Cursor,
                SIZEOFTYPE(:XptCursor), OFFSET(gc_cursor), XtR String, XtN exchange),
            (XtN GarbageCursorForeground, XtC Foreground, XtR Pixel,
                SIZEOFTYPE(:XptPixel), OFFSET(gc_fg), XtR String, XtN XtDefaultForeground),
            (XtN GarbageCursorBackground, XtC Background, XtR Pixel,
                SIZEOFTYPE(:XptPixel), OFFSET(gc_bg), XtR String, XtN XtDefaultBackground),
            (XtN BusyCursor, XtC BusyCursor, XtR Cursor,
                SIZEOFTYPE(:XptCursor), OFFSET(by_cursor), XtR Immediate, 0),
            (XtN BusyCursorForeground, XtC Foreground, XtR Pixel,
                SIZEOFTYPE(:XptPixel), OFFSET(by_fg), XtR String, XtN XtDefaultForeground),
            (XtN BusyCursorBackground, XtC Background, XtR Pixel,
                SIZEOFTYPE(:XptPixel), OFFSET(by_bg), XtR String, XtN XtDefaultBackground),
            6 %},

        AppData = EXPTRINITSTR(:AppData),
    ;

    XptCheckDisplayPtr(dpy)->;

    ;;; get the Busy and the Garbage cursor (use a temporary widget for this)
    fast_XtVaAppCreateShell('Poplog', 'Poplog', xtApplicationShellWidget, dpy, 0) -> w;
    fast_XtVaGetApplicationResources(w, AppData,
            nc_consXptResourceList(explode(appl_resources)->>nres), nres, 0);
    fast_XtDestroyWidget(w);

    exacc AppData.gc_cursor -> garbage_cursor;
    RecolorCursor(dpy, garbage_cursor, exacc AppData.gc_fg, exacc AppData.gc_bg);

    exacc AppData.by_cursor -> busy_cursor;

    ;;; get the busy cursor if its not specified in the defaults
    if busy_cursor == 0 then
        if testdef GetOlBusyCursor then
            weakref GetOlBusyCursor(dpy)
        else
            exacc (2):exptr raw_XCreateFontCursor(dpy, XC_watch)
        endif -> busy_cursor;
    endif;
    RecolorCursor(dpy, busy_cursor, exacc AppData.by_fg, exacc AppData.by_bg);

    conspair(busy_cursor, garbage_cursor) -> cursors;
enddefine;

;;; use a list for display cursors
lvars pdtbl = writeable [%conspair(false,false)%];
define lconstant Get_cursors(dpy);
    lvars dpy;
    Search_list(dpy, pdtbl, Create_cursors) -> pdtbl;
enddefine;

define XptBusyCursor() with_nargs 1;
    BUSY_CURSOR(Get_cursors())
enddefine;
;;;
define updaterof XptBusyCursor() with_nargs 2;
    ;;; trap for XptBusyCursorOn -- returns args
    XptBusyCursorChangeTrap();
    () -> BUSY_CURSOR(Get_cursors())
enddefine;

define global XptGarbageCursor
    = Get_cursors <> GC_CURSOR;
enddefine;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 22 1993
        Avoided creation of XptResourceList structure at compile-time by
        storing fields in a vector and calling nc_consXptResourceList at
        run-time. (POPC can't support creation of shadowclass instances at
        compile-time.)
--- John Gibson, Mar 16 1993
        Added XptBusyCursorChangeTrap instead of having XptBusyCursor vars
--- John Gibson, Sep 21 1991
        Now uses EXPTRINITSTR
--- Jonathan Meyer, Sep  4 1991
        Made STRING_EXPTR lconstant
--- John Gibson, Sep  3 1991
        Added cursor colours.
 */
