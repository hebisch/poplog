/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptListResourceInfo.p
 > Purpose:         Get a list resource database for a widget
 > Author:          Jonathan Meyer, Jan 30 1991 (see revisions)
 > Documentation:   REF *XPT_RESOURCE
 > Related Files:   LIB *XptResourceInfo
 */
compile_mode :pop11 +strict;
section $-Xpt => XptListResourceInfo;

uses XtN;
uses XptResourceInfo;
include xpt_constants

/*
    XptListResourceInfo(widget) -> info_list;
            returns list of all resources known by widget
*/

define global constant XptListResourceInfo(widget);
    lvars widget, class, wid_res, con_res = false;

    if widget.XptDataType == XDT_WIDGETCLASS then
        widget -> class;
        fast_XtInitializeWidgetClass(class);
        Get_class_resources(
            class, ConstraintResources(class), true
        ) ->> con_res -> ConstraintResources(class);
        Get_class_resources(
            class, WidgetResources(class), false
        ) ->> wid_res -> WidgetResources(class),
    else
        ;;; ensure that the properties are up to date
        XptResourceInfo(widget, XtN width) ->;
        fast_XtClass(widget) -> class;
        WidgetResources(class) -> wid_res;
        if fast_XtIsConstraint(fast_XtParent(widget) ->>widget) then
            ConstraintResources(fast_XtClass(widget)) -> con_res;
        endif;
    endif;

    ;;; build list structures

    ;;; this translates the word keys to XtNLookup strings (I would
    ;;; have thought it should produce a list in the same form as
    ;;; accepted by XptSpecifyResourceInfo, i.e. with word names, but the
    ;;; documentation says it returns strings -- JG.)

    define lconstant do_entry(name, val);
        lvars name, val;
        XtNLookup(name, "N") :: (val :: [])
    enddefine;

    [%fast_appproperty(wid_res,do_entry)%] -> wid_res;
    if con_res and length(con_res) /== 0 then
        [%fast_appproperty(con_res,do_entry)%] -> con_res;
    else
        false -> con_res;
    endif;
    [^wid_res ^con_res];
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, May 13 1995
        Now in section $-Xpt
--- John Gibson, Apr 14 1993
        Resource props now keyed on words; added do_entry to map the
        names to strings.
--- Ian Rogers, Feb 17 1993
        Changed calls -Get_class_resources- to coincide with
        LIB * XptResourceInfo
 */
