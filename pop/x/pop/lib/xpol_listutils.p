/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xpol_listutils.p
 > Purpose:         Higher level support for the OpenLook list widget
 > Author:          Jonathan Meyer, Apr 18 1991 (see revisions)
 > Documentation:   REF *OPENLOOK/XpolListItems
 > Related Files:   LIB *OpenLookWidgetSet
 */
compile_mode :pop11 +strict;

uses-now Xol;

section;

exload_batch;

#_IF DEF POPC_COMPILING
#_INCLUDE 'Xol/popc_declare.ph'
#_ENDIF

include xpt_coretypes.ph;
include XolScroll.ph;

uses
    fast_xt_widgetclass,
    xolScrollingListWidget,
;

/* ==== Private Scrolling List Management Functions ====================== */

/*  a single blank OlListItem is used when adding things to a scrollinglist */

lvars
    blank_item = false,
;


/*  table mapping scrollinglist widgets onto a list containing:

        [CURRENT_ITEM ITEM1 ITEM2 ITEM3 ITEM4 ...]

        CURRENT_ITEM is false, or a list_item
*/

lconstant
    list_item_assoc = newproperty([],20, false, "tmparg"),
;

lvars applTouchItem = false, applUpdateView applDeleteItem, applViewItem;

/* scrolling list entries have the following structure : */
defclass lconstant list_item [external_ptr] {
    li_props: full,                 ;;; external_ptr_props
>-> li_exptr: exptr,                ;;; external_ptr
    li_pop_item: full,              ;;; Poplog item equiv of list entry
    li_label: full,                 ;;; printed version of li_pop_item
};


/* Listin:
Generates a list repeater
*/

define lconstant Listin(list);
    lvars list;
    unless list.islist then mishap(list,1,'LIST NEEDED'); endunless;
    define lconstant listin_repeater with_props false;
        lvars item;
        if list == [] then termin
        elseif list.isprocedure then
            list();
        elseif back(list).isprocedure then
            back(list) -> list;
            list();
        else
            if list == [] then termin else dest(list) -> list; endif;
        endif;
    enddefine;
    listin_repeater;
enddefine;


/* Get_widget_list_procs:
Gets procedures from Widget to add and remove items from the scrolling list
*/

define lconstant Get_widget_list_procs(widget);
    lvars widget;
    XptVal[fast] widget(XtN applTouchItem:XptProcedure,
                        XtN applDeleteItem:XptProcedure,
                        XtN applViewItem:XptProcedure,
                        XtN applUpdateView:XptProcedure)
        -> (applTouchItem, applDeleteItem, applViewItem, applUpdateView);
enddefine;


/* Make_label(item)
Prints out item and uses output of print to build fixed address nt string
*/

define lconstant Make_label(item);
    lvars item;
    if item.isstring and item.is_fixed then
        item
    else
        dlocal cucharout = identfn;
        cons_fixed(#| syspr(item) |#, string_key);
    endif;
enddefine;

/* Set_current_item(widget, items, item);
Sets the OL_LIST_ATTR_CURRENT bit for an OlListItem item. stores current
item in list_item_assoc entry -items-.
*/

define lconstant Set_current_item(widget, tags, tag);
    lvars widget, tag, tags, old_tag, item;
    lconstant CLEAR_CURRENT = fi_~~ OL_LIST_ATTR_CURRENT;
    front(tags) -> old_tag;
    if old_tag then
        OlListItemPointer(old_tag) -> item;
        (exacc [fast] :OlListItem item.attr) fi_&& CLEAR_CURRENT
                -> exacc [fast] :OlListItem item.attr;
        exacc [fast] applTouchItem(widget, old_tag);
    endif;
    if tag then
        OlListItemPointer(tag) -> item;
        (exacc [fast] :OlListItem item.attr) fi_|| OL_LIST_ATTR_CURRENT
                -> exacc [fast] :OlListItem item.attr;
        exacc [fast] applTouchItem (widget, tag);
        exacc [fast] applViewItem(widget, tag);
    endif;
    tag -> front(tags);
enddefine;

define lconstant Get_token_pop_item(tag) -> item;
    lvars tag item;
    XptLiveTypeCheck(XptRegister(tag),"OlListToken") -> tag;
    if tag.islist_item then
        tag.li_pop_item
    else
        exacc [fast] :OlListItem (OlListItemPointer(tag)).label
    endif -> item;
enddefine;

/* Set_token_label(widget, token, item) -> token
Sets the label and the item field of token.
*/

define lconstant Set_token_label(widget, token, item);
    lvars widget, token, item, label;
    item -> token.li_pop_item;
    Make_label(item) ->> token.li_label -> label;

    ;;; set OlListItem.label field correctly
    label -> exacc [fast] :OlListItem (OlListItemPointer(token)).label;

    ;;; tell widget we've touched it
    exacc [fast] applTouchItem(widget, token);
enddefine;

define lconstant Set_list_items(widget, repeater, in_tokens);
    lvars tag, widget, repeater
            item, current_items, label, has_changed = false, free, in_tokens;

    list_item_assoc(widget) ->> current_items -> free;
    if current_items then
        /* changing list causes current item to be cleared */
        Set_current_item(widget, current_items, false);
        back(current_items) -> current_items;
    else
        [] -> current_items;
    endif;

    [% fast_repeat
        repeater() -> item;         ;;; returns next thing to put in list
        quitif(item == termin);

        if in_tokens then Get_token_pop_item(item) -> item endif;

        if current_items /== [] then
            ;;; we've got some entries in the scrolling list - so just get
            ;;; the next entry and change its label
            fast_destpair(current_items) -> current_items -> tag;
            if tag.li_pop_item = item then
                tag;
                nextloop;
            endif;
        else
            ;;; need to extend the length of the list, so add a blank item
            ;;; at end:
            unless blank_item then
                consXpolListItemPtr(OL_STRING, nullstring, null_external_ptr,
                    0, null_external_ptr,0) -> blank_item
            endunless;
            XpolAddListItem(widget, false,false, blank_item) -> tag;
            ;;; build new list_item wrapper for item, and register it.
            conslist_item("OlListToken", tag, nullstring, nullstring,
                ) ->> XptRegister(tag) -> tag;
        endif;

        ;;; if we haven't already changed the list, notify the widget that
        ;;; we are about to set some things so it doesn't update itself
        unless has_changed then
            exacc [fast] applUpdateView(widget, 0);
            true -> has_changed;
        endunless;

        ;;; record both item and nt_string version of item to stop then GC'ing
        Set_token_label(widget, tag, item);
        tag;
    endfast_repeat; %] -> item;

    ;;; set list_item_assoc for the new item list
    item /== [] and conspair(false, item) -> list_item_assoc(widget);

    ;;; remove any extra items from end of list
    if current_items /== [] then
        unless has_changed then
            exacc [fast] applUpdateView(widget, 0);
            true -> has_changed;
        endunless;
        fast_for tag in current_items do
            exacc [fast] applDeleteItem(widget, tag);
        endfast_for;
    endif;

    if has_changed then
        ;;; we did something to it - so tell it to update itself
        exacc [fast] applUpdateView(widget, 1);
    endif;

    sys_grbg_list(free);    ;;; free up old current_items list
enddefine;

define lconstant Get_list_items(widget, current_items, wants_tokens);
    lvars widget current_items, wants_tokens;
    returnunless(current_items)(#_<identfn(%termin%)>_#);
    back(current_items) -> current_items;
    define lconstant List_item_repeater;
        if current_items == [] then
            termin
        else
            fast_destpair(current_items) -> current_items;
            unless wants_tokens then li_pop_item(); endunless;
        endif;
    enddefine;
    List_item_repeater;
enddefine;

/* returns a repeater for generating "selected" items */

define lconstant Get_selected_items(widget, current_items, wants_tokens);
    lvars widget current_items, wants_tokens;
    returnunless(current_items)(#_<identfn(%termin%)>_#);
    back(current_items) -> current_items;

    define lconstant List_item_repeater;
        lvars item;
        repeat;
            returnif(current_items == [])(termin);
            fast_destpair(current_items) -> current_items -> item;
            if exacc [fast] :OlListItem
                (OlListItemPointer(item)).attr &&/=_0 OL_LIST_ATTR_SELECTED
            then
                unless wants_tokens then li_pop_item(item) -> item; endunless;
                return(item);
            endif;
        endrepeat;
    enddefine;

    List_item_repeater;
enddefine;

/* Get_item_token(widget, current_items, item) -
returns the list_item associated with -item- in current_items
*/

define lconstant Get_item_token(widget, current_items, item);
    lvars widget, item, current_items, token;
    if current_items then
        fast_back(current_items) -> current_items;

            /* search with == */
        fast_for token in current_items do
            returnif(token.li_pop_item == item)(token);
        endfast_for;

            /* search with = */
        fast_for token in current_items do
            returnif(token.li_pop_item = item)(token);
        endfast_for;
    endif;
            /* no current item; return false */
    false;
enddefine;

/* ==== Public Interface to List =================================== */

/* Check_widget(w) -
Checks for subclasses of OLIT ScrollingList
*/

define lconstant Check_widget(widget);
    lvars widget;
    unless applTouchItem then Get_widget_list_procs(widget); endunless;
    unless fast_XtIsSubclass(XptCheckWidget(widget), xolScrollingListWidget)
    then
        mishap(widget,1,'OpenLook ScrollingListWidget NEEDED');
    endunless;
enddefine;

define XpolSelectedListItems(widget) -> repeater;
    lvars widget, repeater, wants_tokens = false, items;
    if widget.isboolean then widget -> (widget, wants_tokens) endif;
    Check_widget(widget);
    list_item_assoc(widget) -> items;
    pdtolist(Get_selected_items(widget, items, wants_tokens)) -> repeater;
enddefine;

define XpolListItems(widget) -> repeater;
    lvars widget, repeater, wants_tokens = false, items;
    if widget.isboolean then widget -> (widget, wants_tokens) endif;
    Check_widget(widget);
    list_item_assoc(widget) -> items;
    pdtolist(Get_list_items(widget, items, wants_tokens)) -> repeater;
enddefine;

define updaterof XpolListItems(repeater, widget);
    lvars widget, repeater, in_tokens = false;
    if widget.isboolean then
        (repeater,widget) -> (repeater,widget, in_tokens);
    endif;
    Check_widget(widget);
    unless repeater.isprocedure then Listin(repeater) -> repeater endunless;
    Set_list_items(widget, repeater, in_tokens);
enddefine;

define XpolListTokenToItem = Get_token_pop_item enddefine;

define XpolListItemToToken(widget, item);
    lvars widget, item;
    Check_widget(widget);
    Get_item_token(widget, list_item_assoc(widget), item);
enddefine;

define XpolCurrentListItem(widget);
    lvars widget, current_items, item, wants_token = false;
    if widget.isboolean then widget -> (widget, wants_token) endif;
    Check_widget(widget);
    list_item_assoc(widget) -> current_items;
    if current_items and front(current_items) ->> current_items then
        current_items,
        unless wants_token then li_pop_item() endunless;
    else
        false
    endif;
enddefine;

define updaterof XpolCurrentListItem(item, widget);
    lvars item, widget, current_items;
    if widget.isboolean then (item,widget) -> (item,widget,); endif;
    Check_widget(widget);
    returnunless(list_item_assoc(widget) ->> current_items);
    if isexternal_ptr_class(item) then XptRegister(item) -> item endif;
    unless item.islist_item then
        Get_item_token(widget, current_items, item) -> item;
    endunless;
    if item and not(fast_lmember(item, current_items.back)) then
        mishap(widget, item, 2,'ITEM NOT IN LIST');
    endif;
    Set_current_item(widget, current_items, item);
enddefine;

define XpolSubscrListItems(n, widget) -> item;
    lvars n, widget, item, current_items, wants_token = false;
    if widget.isboolean then (n,widget) -> (n, widget, wants_token) endif;
    Check_widget(widget);
    list_item_assoc(widget) -> current_items;
    unless current_items then
        mishap(widget, 1, 'NO ITEMS IN LIST');
    endunless;
    subscrl(n, back(current_items)) -> item;
    unless wants_token then li_pop_item(item) -> item endunless;
enddefine;

define updaterof XpolSubscrListItems(item, n, widget);
    lvars item n widget current_items;
    if widget.isboolean then (item,n,widget) -> (item,n,widget,); endif;
    Check_widget(widget);
    list_item_assoc(widget) -> current_items;
    unless current_items then
        mishap(widget, 1, 'NO ITEMS IN LIST');
    endunless;
    subscrl(n, back(current_items)) -> n;
    Set_token_label(widget, n, item);
enddefine;

define XpolNumListItems(widget);
    lvars widget, current_items;
    Check_widget(widget);
    list_item_assoc(widget) -> current_items;
    if current_items then listlength(back(current_items)) else 0 endif;
enddefine;

constant xpol_listutils = true; /* for uses */

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 12 1993
        o  Uses xolScrollingListWidget instead of XptW*idgetSet
        o  blank_item now an lvars initialised at runtime (POPC can't
           handle compile-time construction of shadowclasses).
--- John Gibson, Sep 11 1992
        Changed to use XptVal
--- Jonathan Meyer, Jun 16 1992
    Renamed things starting with XpOL -> Xpol (how they are documented).
--- Adrian Howard, Feb 21 1992 : Installed JonM's changes enabling the
        library to work with OLIT 2.5+
--- Adrian Howard, Nov  1 1991 : Changed to use -XpolListItemPtr-.
--- Jonathan Meyer, Sep  5 1991
        Fixed bug in Set_list_items
--- Jonathan Meyer, Aug  6 1991
        Added XpolSelectedListItems
        Added XpolCurrentListItem, XpolListItemToToken, XpolSubscrListItems
--- Jonathan Meyer, Jul  6 1991
        Changed to use null_external_ptr
 */
