/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xpw/XpwGraphic.p
 > Purpose:         Methods in support of XpwGraphic widgetclass
 > Author:          Jon Meyer, Jan 1990 (see revisions)
 > Documentation:   REF * XpwGraphic, HELP * XpwGraphic
 > Related Files:   LIB * XpwGraphic, * XpwGraphic.ph
 */
compile_mode:pop11 +strict;

include XpwGraphic.ph;
include xpt_xcolor.ph;

section;
exload_batch;

uses
    fast_xt_display,
    XpwPixmap,
    xpwGraphicWidget,
;

;;; arg-checking interface to external methods
/*
typedef struct _XpwColorList {
    XColor *colors;
    Cardinal num_colors;
    Colormap cmap;
    int flags;
    struct _XpwColorList *next;
} XpwColorList;
*/

l_typespec
    xcolor :XColor,
    XpwColor {
        colors: exptr.:xcolor[],
        num_colors: long,
        colormap: long,
        flags: long,
    };

XptLoadProcedures XpwGraphic lvars XQueryColor;

define XpwStackColorRangeInfo(range, i);
    lvars range, i, colors, xcolor;
    fi_check(i, 1, exacc :XpwColor range.num_colors)->;
    exacc (exacc :XpwColor range.colors)[i] -> xcolor;
    exacc xcolor.pixel;
    exacc xcolor.red >> 8;
    exacc xcolor.green >> 8;
    exacc xcolor.blue >> 8;
enddefine;

define XpwColorRangeInfo() with_nargs 2;
    XpwStackColorRangeInfo();
    consvector(4);
enddefine;

define XpwColorRangeBase(range);
    lvars range, colors, xcolor;
    exacc :xcolor (exacc (exacc :XpwColor range.colors)[1]).pixel;
enddefine;

define XpwAllocColorRange(w, num_cells, r1,g1,b1,r2,g2,b2);
    lvars w num_cells,r1,g1,b1,r2,g2,b2;
    XpwCallMethod(XptCheckWidget(w),XpwMAllocColorRange,
                fi_check(num_cells,0,false), fi_check(r1,0,255),
                fi_check(g1,0,255), fi_check(b1,0,255),
                fi_check(r2,0,255), fi_check(g2,0,255),
                fi_check(b2,0,255), 9, "exptr");
enddefine;

define XpwFreeColorRange(w, range);
    lvars w,range,base;
    XpwCallMethod(XptCheckWidget(w), XpwMFreeColorRange, range,3,false);
enddefine;

define XpwCreateColormap(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w), XpwMCreateColormap, 2, false);
enddefine;

define XpwFreeColormap(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w), XpwMFreeColormap, 2, false);
enddefine;

define XpwAllocColor(w,r,g,b);
    lvars w,r,g,b;
    XpwCallMethod(XptCheckWidget(w), XpwMAllocStoreColor,
            fi_check(r,0,255),fi_check(g,0,255),
            fi_check(b,0,255),5,"XptPixel");
enddefine;

define XpwChangeColor(w,p,r,g,b);
    lvars w,p,r,g,b;
    XpwCallMethod(XptCheckWidget(w),XpwMSetPixelColor,
        fi_check(p,false,false),fi_check(r,0,255),
        fi_check(g,0,255),fi_check(b,0,255),6,false);
enddefine;

define XpwQueryColor(widget, pixel);
    lvars widget, pixel;
    lconstant xcolor = exptr_init_fixed(SIZEOFTYPE(:XColor), string_key);
    pixel -> exacc xcolor.pixel;

    exacc [fast] (3) raw_XQueryColor(fast_XtDisplay(XptCheckWidget(widget)),
                XptVal[fast] widget(XtN colormap), xcolor);

    (exacc [fast] xcolor.red) fi_>> 8;
    (exacc [fast] xcolor.green) fi_>> 8;
    (exacc [fast] xcolor.blue) fi_>> 8;
enddefine;

constant XpwGraphic = true;

endexload_batch;
endsection;     /* top level */

/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  6 1993
        Uses Xpw/xpwGraphicWidget
--- John Gibson, Sep 11 1992
        Changed to use XptVal
--- Adrian Howard, Jan 23 1992 : Added -XpwStackColorRangeInfo-
--- Adrian Howard, Nov  4 1991 : Now compiles under +strict
--- Jonathan Meyer, Sep 25 1991 Added XpwQueryColor.
--- Jonathan Meyer, Jul 30 1991
        Changed to use new XpwCallMethod interface, which fixes bug in
        XpwFailureImport. Removed calls to (redundant) xpw_check_live_widget
--- Ian Rogers, Apr 17 1991
        Changed hard-code exacc to call of -XpwImportXID-
--- Jonathan Meyer, Mar 13 1991
        Changed reps of resources in XptSpecifyResourceInfo so that
        they don't get specified as typed args (the X toolkit cannot
        cope with typed subpart resources)
--- Jonathan Meyer, Mar 13 1991 Added XptSpecifyResourceInfo to
    make GC subpart resources available from XptPopValue
--- Jonathan Meyer, Feb 13 1991 Changed XpwAllocColor so it doesn't use
        XpwFailureImport
--- Jonathan Meyer, Jan 29 1991
        Added XpwColorRangeBase and XpwColorRangeInfo
        Removed XpwColorRangePixel and XpwColorRangeFlags
--- Ian Rogers, Jan 18 1991
        Replaced and hard-coded failure-test code with -XpwFailureImport-
--- Jonathan Meyer, Oct 23 1990
        Added XptWidgetSet("Poplog")("PixmapWidget") line to demand load
        XpwPixmap. Removed uses X11/StringDefs.
--- Roger Evans, Oct 22 1990
    removed use of external_ptr_field, renamed XpwGraphic and installed
--- James Goodlet, Sep 27 1990 - added definition of XpwGraphicMethods
        for -uses-.
--- Jonathan Meyer, Aug 31 1990
    Changed to make it work with new xpop: added #_IF DEF XptWidgetSet
    removed XpwSwitchColormaps, removed xt_ from xpw_check_live_widget
--- Jonathan Meyer, Jul 25 1990
    Added dlocal line to XpwSetColor/Font/Cursor because of bug in 13.82.
    Should be removed when bug is fixed.
--- Andreas Schoter, July 16 1990
    Renamed to XpwGraphicMethods.p and changed all variable names from Pop* to
    Xpw*
--- James Goodlet, Jun  6 1990 - moved out from LIB * PopGraphic - see that
        file for more revision notes.
 */
