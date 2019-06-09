/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/ved/lib/S-xvedS-scrollbar_xol.p
 > Purpose:         Scrollbar defines for OpenLook XVed
 > Author:          John Gibson, Jun  1 1993 (see revisions)
 */
compile_mode :pop11 +strict;

uses-now Xol;

section $-xved;

#_INCLUDE '$usepop/pop/x/ved/src/gui.ph'
include xpt_coretypes.ph;
include XolScroll.ph;

uses
    $-xved$-create_parent_xol,  ;;; force this come in
    xolScrollbarWidget,
    xolMenuShellWidget,
;

/*  Vector of resource names and procedures whose elements are
 *  assigned in this file -- see gui.ph
 */
lconstant scrollbar_gui_switch_vec = INIT_scrollbar_gui_switch_vec;
constant scrollbar_xol   = scrollbar_gui_switch_vec;


xolScrollbarWidget  -> wc_SCROLLBAR;

[   ^XtN proportionLength   1
    ^XtN yResizable         ^true
    ^XtN yAttachBottom      ^true
    ^XtN yAddHeight         ^true
    ^XtN width              18
]                   -> l_SCROLLBAR_ARGS;

[   ^XtN proportionLength   1
    ^XtN xResizable         ^true
    ^XtN xAttachRight       ^true
    ^XtN yAttachBottom      ^true
    ^XtN xAddWidth          ^true
    ^XtN yAddHeight         ^true
    ^XtN height             18
    ^XtN orientation        ^OL_HORIZONTAL
]                   -> l_HSCROLLBAR_ARGS;

XtN sliderMoved     -> n_ScrollbarMoved;
false               -> n_ScrollbarDragged;

define :macexpr p_SET_SCROLLBAR(sb, Max, Value, Len, Incr, Min);
    Max, Value, Len, Min
        -> XptVal[fast] sb( XtN sliderMax <OPT>,
                            XtN sliderValue <OPT>,
                            XtN proportionLength <OPT>,
                            XtN sliderMin <OPT>);
enddefine;

define :macexpr p_SCROLLBAR_VALUE(sb);
    XptVal[fast] sb(XtN sliderValue)
enddefine;

;;; If the widget is a scrollbar, return the MenuShell associated with its
;;; menuPane resource, false otherwise. Used to set resource values in
;;; xvedresources.p and xvedscrollbar.p
define :macexpr p_HAS_SCROLLBAR_MENU(w);
    if fast_XtClass(w) == xolScrollbarWidget then
        XptVal[fast] w(XtN menuPane:XptWidget) -> w;
        until fast_XtClass(w) == xolMenuShellWidget do
            fast_XtParent(w) -> w;
        enduntil;
        w;
    else
        false;
    endif;
enddefine;

/* callback verify */
define :macexpr p_SCROLL_CB_VERIFY(w, call);
    l_typespec call :OlScrollbarVerify;
    true -> exacc call.ok;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep  1 1997
        Added additional arg to p_SET_SCROLLBAR.
--- John Gibson, Aug 11 1994
        Added yAttachBottom true for l_HSCROLLBAR_ARGS
--- John Gibson, Feb 11 1994
        Added l_HSCROLLBAR_ARGS
--- John Gibson, Dec 23 1993
        Improved interface to xvedscrollbar.p
 */
