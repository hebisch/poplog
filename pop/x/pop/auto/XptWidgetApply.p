/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptWidgetApply.p
 > Purpose:         Class apply for widgets
 > Author:          Jonathan Meyer, Jun 27 1991 (see revisions)
 > Documentation:   REF *XPT_CLASSAPPLY
 > Related Files:   LIB *XptScreenPtrApply LIB *XptWidgetClassApply
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include xpt_coretypes;

uses fast_xt_widget;
uses fast_xt_widgetclass;
uses fast_xt_widgetinfo;
uses fast_xt_composite;
uses XptPopValue;
uses XptChildren;
uses XptWidgetTree;

define XptWidgetApply(name, widget);
    lvars widget name children num_children result;
    l_typespec children :XptWidget[];
    lconstant map = [
        name ^XptDataProps
        children ^XptChildren
        shell ^XptShellOfObject
        ancestors ^XptAncestors
        parent ^fast_XtParent
        class ^fast_XtClass
        superclass ^fast_XtSuperclass
        window ^fast_XtWindowOfObject
        screen ^fast_XtScreenOfObject
        display ^fast_XtDisplayOfObject
        appcontext ^fast_XtWidgetToApplicationContext
        realized ^fast_XtIsRealized
        managed ^fast_XtIsManaged
        tree ^XptWidgetTree
    ];

    XptCheckWidget(widget)->;

    if name.isstring then
        ;;; get resource
        XptPopValue(widget, name)
    elseif name.isinteger then
        ;;; get name'th child
        if fast_XtIsComposite(widget) then
            XptVal[fast] widget(XtN children:exptr, XtN numChildren)
                                -> (children, num_children);
            if name fi_>= 1 and name fi_<= num_children then
                exacc [fast] children[name]
            else
                false
            endif;
        else
            false;
        endif;
    elseif fast_lmember(name, map) ->> result then
        fast_apply(widget, fast_front(fast_back(result)))
    elseif name.isword then
        ;;; see if its a resource
        XptPopValue(widget, XtNLookup(name, "N"));
    else
        mishap(name,1,'UNKNOWN Widget FIELD');
    endif;
enddefine;


define updaterof XptWidgetApply(val,name,widget);
    lvars val name widget;
    XptCheckWidget(widget)->;
    if name.isstring then
        ;;; performs checking
        val -> XptPopValue(widget, name);
    elseif name.isword then
        if name == "managed" then
            if val then fast_XtManageChild(widget)
            else fast_XtUnmanageChild(widget)
            endif;
        elseif name == "realized" then
            if val then fast_XtRealizeWidget(widget)
            else fast_XtUnrealizeWidget(widget)
            endif;
        else
            val -> XptPopValue(widget, XtNLookup(name, "N"));
        endif;
    else
        mishap(name,1,'UNKNOWN Widget FIELD');
    endif;
enddefine;

endexload_batch;
endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Sep 10 1992
        Added "tree" option to return the widget-tree rooted at the specified
        widget
--- John Gibson, Sep  7 1992
        Changed to use XptVal
--- Adrian Howard, Nov  1 1991 : Removed reference to -typespec:XptWidgetList-
--- Jonathan Meyer, Jul  5 1991 : Added "shell" type
 */
