/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptChildren.p
 > Purpose:         Returns list of direct children of widget
 > Author:          Tom Khabaza, Ian Rogers, Apr 24 1991 (see revisions)
 > Documentation:   REF *XT_LIBS/XptChildren
 > Related Files:   LIB *XptAncestors
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses fast_xt_widgetinfo;
uses fast_xt_widgetclass;

include xpt_coretypes.ph;

define global XptChildren(widget);
    lvars i children widget num_children;
    l_typespec children :XptWidget[];

    [%if fast_XtIsComposite(XptCheckWidget(widget)) then
        XptVal[fast] widget(XtN children:exptr, XtN numChildren:int)
                        -> (children, num_children);
        fast_for i from 1 to num_children do
            exacc [fast] children[i];
        endfast_for
    endif %];
enddefine;

endexload_batch;
endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Sep  7 1992
        Added missing variable declaration
--- John Gibson, Sep  7 1992
        Changed to use XptVal
--- Jonathan Meyer, Jul  5 1991
        Rewrote to work efficiently (as these things go)
--- Ian Rogers, Jun  4 1991
    Added -uses xt_widget-
 */
