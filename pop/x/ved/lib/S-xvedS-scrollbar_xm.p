/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/ved/lib/S-xvedS-scrollbar_xm.p
 > Purpose:         Scrollbar defines for Motif XVed
 > Author:          John Gibson, Jun  1 1993 (see revisions)
 */
compile_mode :pop11 +strict;

uses-now Xm;

section $-xved;

#_INCLUDE '$usepop/pop/x/ved/src/gui.ph'
include XmConstants.ph;

uses
    $-xved$-create_parent_xm,   ;;; force this come in
    xmScrollBarWidget,
;

/*  Vector of resource names and procedures whose elements are
 *  assigned in this file -- see gui.ph
 */
lconstant scrollbar_gui_switch_vec = INIT_scrollbar_gui_switch_vec;
constant scrollbar_xm   = scrollbar_gui_switch_vec;


xmScrollBarWidget       -> wc_SCROLLBAR;

;;; Don't wish scrollbar to receive keyboard focus
[   ^XtN traversalOn    ^false
    ^XtN navigationType ^XmNONE
]                       -> l_SCROLLBAR_ARGS;

[   ^XtN orientation    ^XmHORIZONTAL
] <> l_SCROLLBAR_ARGS   -> l_HSCROLLBAR_ARGS;

XtN valueChangedCallback-> n_ScrollbarMoved;
XtN dragCallback        -> n_ScrollbarDragged;

define :macexpr p_SET_SCROLLBAR(sb, Max, Value, Len, Incr, Min);
    Max,  Value,  Len,  Incr,  Min,  Len and max(1, Len fi_- 1)
        -> XptVal[fast] sb( XmN maximum <OPT>,
                            XmN value <OPT>,
                            XmN sliderSize <OPT>,
                            XmN increment <OPT>,
                            XmN minimum <OPT>,
                            XmN pageIncrement <OPT>);
enddefine;

define :macexpr p_SCROLLBAR_VALUE(sb);
    lvars sb;
    XptVal[fast] sb(XmN value)
enddefine;

define :macexpr p_HAS_SCROLLBAR_MENU(sb);
    lvars sb;
    ;;; No menus on Motif scrollbars
    false
enddefine;

/* callback */
define :macexpr p_SCROLL_CB_VERIFY(sb, call);
    lvars sb, call;
    ;;; nothing to do
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep  1 1997
        Added additional Incr arg to p_SET_SCROLLBAR for setting
        XmN increment on scrollbar.
--- John Gibson, Feb 11 1994
        Added l_HSCROLLBAR_ARGS
--- John Gibson, Dec 23 1993
        Improved interface to xvedscrollbar.p
 */
