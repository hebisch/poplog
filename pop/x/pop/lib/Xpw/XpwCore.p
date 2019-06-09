/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xpw/XpwCore.p
 > Purpose:         Methods in support of XpwCore widgetclass
 > Author:          Jon Meyer, Jan 1990 (see revisions)
 > Documentation:   REF * XpwCore, HELP * XpwCore
 > Related Files:   LIB * XpwCore, * XpwCore.ph
 */
compile_mode:pop11 +strict;

section;
exload_batch;

include XpwCore.ph;
include xpt_xfontstruct.ph;

uses
    fast_xt_display,
    XpwCallMethod,
    xpwCoreWidget,
;

XptLoadProcedures XpwCore
lvars
    XFreePixmap(dpy,pixmap),
    XmbTextEscapement(font_set,string,num_bytes) :int,
    XExtentsOfFontSet(font_set) :exptr.{:XFontSetExtents},
;


define XpwSetFont(w, string);
    lvars w, string;
    XpwCallMethod(XptCheckWidget(w), XpwMSetFont,
            XptCheckString(string), 3, "XptXID")
enddefine;

define XpwFreeFont(w,string);
    lvars w, string;
    XpwCallMethod(XptCheckWidget(w), XpwMFreeFont,
                XptCheckString(string), 3, false);
enddefine;

define XpwSetCursor(w, shape);
    lvars w, shape, c;
    XpwCallMethod(XptCheckWidget(w), XpwMSetCursor,
                XptCheckUnsignedInt(shape), 3, "XptXID");
enddefine;

define XpwFreeCursor(w, shape);
    lvars w, shape, c;
    XpwCallMethod(XptCheckWidget(w), XpwMFreeCursor,
                XptCheckUnsignedInt(shape), 3, false);
enddefine;

define XpwSetColor(w, string);
    lvars w, string;
    XpwCallMethod(XptCheckWidget(w), XpwMSetColor,
                XptCheckString(string), 3, "XptPixel");
enddefine;

define XpwFreeColor(w, string);
    lvars w, string;
    XpwCallMethod(XptCheckWidget(w), XpwMFreeColor,
                XptCheckString(string), 3, false);
enddefine;

define XpwChangeUsersGC(w, valuemask, values);
    lvars w, valuemask, values;
    unless values.is_valid_external_ptr then
        mishap(values,1,'LIVE EXTERNAL PTR NEEDED');
    endunless;
    XpwCallMethod(XptCheckWidget(w), XpwMChangeUsersGC,
                XptCheckUnsignedInt(valuemask), values, 4, false);
enddefine;


define XpwLoadPixmap(w, name, foreground, background, depth);
    lvars w, name, foreground, background, depth;
    XpwCallMethod(XptCheckWidget(w), XpwMLoadPixmap,
                XptCheckString(sysfileok(name, false)),
                XptCheckInt(foreground),
                XptCheckInt(background),
                XptCheckUnsignedInt(depth), 6, "XptXID");
enddefine;

;;; Cached version of above
lconstant
        key = writeable {0 0 0 0 0},
        table = newanyproperty([], 10, false, false,
                            syshash, sys_=, "perm", false, false),
    ;

define XpwGetPixmap(w, name, fg, bg, depth);
    lvars w, name, fg, bg, depth, pmap;
    if depth == 0 then
        ;;; need to do this here so that we put the right thing in the cache
        XptVal w(XtN depth) -> depth;
    endif;

    ;;; keyed on display, filename, colours and depth
    fast_XtDisplay(w), name, fg, bg, depth -> explode(key);

    if table(key) ->> pmap then
        pmap;
    elseif XpwLoadPixmap(w, name, fg, bg, depth) ->> pmap then
        pmap ->> table(copy(key));  ;;; must copy since key is constant
    else
        false
    endif;
enddefine;

define XpwFreePixmap(w, pixmap);
    lvars w, pixmap, name, val;
    fast_for name, val in_property table do
        if pixmap == val then
            false -> table(name);
        endif;
    endfast_for;
    exacc raw_XFreePixmap(fast_XtDisplay(XptCheckWidget(w)), pixmap);
enddefine;



/* Xpw Font Bounds Utilities */

define :inline lconstant FONTSET(w);
    XptVal[nc] w(XtN fontSet:exptr)
enddefine;

/* Returns width in pixels for string */

define XpwTextWidth(w, string);
    lvars s = sys_encode_string_fixed(string);
    exacc raw_XmbTextEscapement(FONTSET(w), s, datalength(s));
    if s /== string then sys_grbg_fixed(s) endif
enddefine;

/* Return logical ascent,descent & height for Xpw fontset */

define XpwFontMaxLogicalExtent(w) /* -> (x, y, width, height) */;
    lvars XRectangle = exacc[nc] (exacc[nc] raw_XExtentsOfFontSet(FONTSET(w)))
                                            .max_logical_extent;
    exacc[fast] XRectangle.x, exacc[fast] XRectangle.y,
    exacc[fast] XRectangle.width, exacc[fast] XRectangle.height
enddefine;

define XpwFontAscent(w);
    lvars (_, y, _, _) = XpwFontMaxLogicalExtent(w);
    -y
enddefine;

define XpwFontDescent(w);
    lvars (_, y, _, height) = XpwFontMaxLogicalExtent(w);
    y + height
enddefine;

define XpwFontHeight(w);
    lvars (_, _, _, height) = XpwFontMaxLogicalExtent(w);
    height
enddefine;

constant XpwCore = true;

endexload_batch;
endsection;     /* top level */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 12 1997
        Added XpwFontMaxLogicalExtent, and changed the 3 other Font
        procedures to use it.
--- John Gibson, Apr  8 1997
        Rewrote XpwTextWidth, XpwFontAscent, XpwFontDescent and XpwFontHeight
        to use the X font set functions with XtNfontSet.
--- John Gibson, Apr  2 1993
        Added uses for xpwCoreWidget
--- Adrian Howard, Nov 11 1992
        Added optional arg to sysfileok in XpwLoadPixmap to prevent wrong
        error if filename passed as false
--- John Gibson, Sep 11 1992
        Changed to use XptVal
--- Adrian Howard, Nov  4 1991 : Added +strict compile mode.
--- Jonathan Meyer, Oct  4 1991 Added XpwChangeUsersGC
--- Jonathan Meyer, Sep 25 1991 XpwLoadPixmap foreground/background
        changed to be Ints instead of UnsignedInts
--- Jonathan Meyer, Sep 11 1991 Made working vectors writeable
--- Jonathan Meyer, Sep  3 1991 Fixed declaration in XpwCore.p
--- Jonathan Meyer, Jul 31 1991
        Made key a vector
--- Jonathan Meyer, Jul 30 1991
        Changed to use new XpwCallMethod interface, which imports XptPixel
        values correctly (removed XpwFailureImport which didn't work).
        Changed calls of xpw_check_live_widget to XptCheckWidget
--- Jonathan Meyer, Jul 29 1991
        Added XpwFont* procedures, and XpwLoadPixmap/XpwFreePixmap/XpwGetPixmap
--- Ian Rogers, Apr 17 1991
        Replaced bogus calls of -XpwFailureImport- with -XpwImportXID-
--- Ian Rogers, Jan 18 1991
        Replaced and hard-coded failure-test code with -XpwFailureImport-
--- Roger Evans, Oct 22 1990 changed to XpwCore and reinstalled
--- James Goodlet, Sep 27 1990 - added definition of XpwCoreMethods
        for -uses-.
--- Jonathan Meyer, Aug 31 1990
    Added #_IF DEF XptWidgetSet to check if we are working under new xpop.
--- Jonathan Meyer, Jul 25 1990
    Added dlocal line to XpwSetColor/Font/Cursor because of bug in 13.82.
    Should be removed when bug is fixed.
--- Andreas Schoter, July 16 1990
    Renamed to XpwCoreMethods.p and changed all variable names from Pop* to
    Xpw*
--- James Goodlet, Jun  6 1990 - moved out from LIB * PopCore - see that
        file for more revision notes.
 */
