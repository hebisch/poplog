/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xpm_listutils.p
 > Purpose:         Higher level support for the Motif list widget
 > Author:          Jonathan Meyer, Jan 15 1992 (see revisions)
 > Documentation:   REF *MOTIF/XpmListItems
 > Related Files:   LIB *MotifWidgetSet
 */
compile_mode :pop11 +strict;

uses-now Xm;

section;

exload_batch;

#_IF DEF POPC_COMPILING
#_INCLUDE 'Xm/popc_declare.ph'
#_ENDIF

include XmConstants.ph;

uses xmListWidget;


/* ==== Private Scrolling List Management Functions ====================== */

/*  table mapping scrollinglist widgets onto a vector of strings
*/

lconstant
    list_item_assoc = newproperty([],20, false, "tmparg"),
;

/* scrolling list entries have the following structure : */
defclass lconstant list_item [external_ptr] {
    li_props: full,                 ;;; external_ptr_props
>-> li_exptr: exptr,                ;;; external_ptr
    li_descriptor: full,            ;;; hold on descriptor
    li_item: full,                  ;;; Poplog item equiv of list entry
    li_label: full,                 ;;; printed version of li_item
};

defclass lconstant list_item_array :exptr;

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

define lconstant View_item(w, item);
    lvars w, item,
     (top, count) = XptVal[fast] w(XtN topItemPosition, XtN visibleItemCount);
    unless item.isinteger then
        XmListItemPos(w, item) -> item;
    endunless;
    if item < top then
        XmListSetPos(w, item);
    elseif item >= top+count then
        XmListSetBottomPos(w, item);
    endif;
enddefine;

define lconstant Get_token_item(tag) -> item;
    lvars tag item;
    XptLiveTypeCheck(XptRegister(tag),"XmString") -> tag;
    if tag.islist_item then
        tag.li_item
    else
        XpmCoerceString(tag)
    endif -> item;
enddefine;

define lconstant Set_list_items(widget, repeater, in_tokens);
    lvars tag, widget, repeater, i = 0, items, num_current_items,
            item, current_items, label, strings, in_tokens;

    list_item_assoc(widget) -> current_items;
    current_items and datalength(current_items) or -1 -> num_current_items;
    {% fast_repeat
        repeater() -> item;         ;;; returns next thing to put in list
        quitif(item == termin);
        if in_tokens then Get_token_item(item) -> item endif;

        if i fi_< num_current_items then
            i fi_+1 -> i;
            subscrv(i, current_items) -> tag;
            if tag.li_item = item then
                ;;; reuse the compound string
                item -> tag.li_item;
                tag;
                nextloop;
            endif;
        endif;
        item sys_>< '' -> label;
        (->XpmCoerceString(label)) -> tag;
        ;;; build new list_item wrapper for item, and register it.
        conslist_item("XmString", tag, tag, item, label)
                    ->> XptRegister(tag) ->> tag;
        XmStringFree -> sys_destroy_action(tag);
    endfast_repeat; %} ->> items -> list_item_assoc(widget);
    datalength(items) -> i;
    conslist_item_array(items.explode, i) -> strings;
    if i == num_current_items then
        XmListReplaceItemsPos(widget, strings, i, 1);
    else
        fast_XtVaSetValues(widget, XmN items, strings, XmN itemCount, i, 4);
    endif;
    XmListDeselectAllItems(widget);
enddefine;

define lconstant Get_list_items(widget, current_items, wants_tokens);
    lvars widget current_items, wants_tokens;
    returnunless(current_items)(#_<identfn(%termin%)>_#);
    [%explode(current_items)%] -> current_items;
    define lconstant List_item_repeater;
        if current_items == [] then
            termin
        else
            fast_destpair(current_items) -> current_items;
            unless wants_tokens then li_item(); endunless;
        endif;
    enddefine;
    List_item_repeater;
enddefine;

/* returns a repeater for generating "selected" items */

define lconstant Get_selected_items(widget, current_items, wants_tokens);
    lvars widget, items, num, current_items, wants_tokens, i;
    returnunless(current_items)(#_<identfn(%termin%)>_#);
    lconstant ptr = EXPTRINITSTR(:exptr), count = EXPTRINITSTR(:int);
    unless XmListGetSelectedPos(widget, ptr, count) then
        return(#_<identfn(%termin%)>_#);
    endunless;
    exacc :exptr ptr -> items; exacc :int count -> num;
    l_typespec items :uint[];
    [%fast_for i from 1 to num do
        subscrv(exacc [fast] items[i], current_items)
    endfast_for %] -> current_items;
    fast_XtFree(items);
    define lconstant List_item_repeater;
        lvars item;
        repeat;
            returnif(current_items == [])(termin);
            fast_destpair(current_items) -> current_items -> item;
            unless wants_tokens then Get_token_item(item) -> item; endunless;
            return(item);
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
            /* search with == */
        fast_for token in_vector current_items do
            returnif(token.li_item == item)(token);
        endfast_for;

            /* search with = */
        fast_for token in_vector current_items do
            returnif(token.li_item = item)(token);
        endfast_for;
    endif;
            /* no current item; return false */
    false;
enddefine;

define lconstant Get_item_token_pos(widget, current_items, item);
    lvars widget, item, current_items, num = datalength(current_items), i, t;
    if current_items then
            /* search with == */
        fast_for i from 1 to num do
            fast_subscrv(i, current_items) -> t;
            returnif(t == item or t.li_item == item)(i);
        endfast_for;

            /* search with = */
        fast_for i from 1 to num do
            returnif(fast_subscrv(i, current_items).li_item = item)(i);
        endfast_for;
    endif;
            /* no current item; return false */
    false;
enddefine;

/* ==== Public Interface to List =================================== */

/* Check_widget(w) -
Checks for subclasses of MOTIF ListWidget
*/

define lconstant Check_widget(widget);
    lvars widget;
    unless XmIsList(widget) then
        mishap(widget,1,'Motif ListWidget Needed');
    endunless;
enddefine;

define XpmSelectedListItems(widget) -> repeater;
    lvars widget, repeater, wants_tokens = false, items;
    if widget.isboolean then widget -> (widget, wants_tokens) endif;
    Check_widget(widget);
    list_item_assoc(widget) -> items;
    pdtolist(Get_selected_items(widget, items, wants_tokens)) -> repeater;
enddefine;

define updaterof XpmSelectedListItems(repeater, widget);
    lvars item, token, widget, repeater, one_only, in_tokens = false,
                current_items;
    if widget.isboolean then
        (repeater,widget) -> (repeater,widget, in_tokens);
    endif;
    Check_widget(widget);
    XptVal[fast] widget(XtN selectionPolicy:byte) == XmSINGLE_SELECT
            -> one_only;
    unless repeater.isprocedure then Listin(repeater) -> repeater endunless;
    list_item_assoc(widget) -> current_items;
    XmListDeselectAllItems(widget);
    fast_repeat;
        repeater() ->> item -> token;
        quitif(item == termin);
        Get_item_token_pos(widget, current_items, token) -> token;
        unless token then mishap(item,1,'ITEM NOT IN LIST') endunless;
        XmListSelectPos(widget, token, false);
        quitif(one_only);
    endfast_repeat;
enddefine;

define XpmListItems(widget) -> repeater;
    lvars widget, repeater, wants_tokens = false, items;
    if widget.isboolean then widget -> (widget, wants_tokens) endif;
    Check_widget(widget);
    list_item_assoc(widget) -> items;
    pdtolist(Get_list_items(widget, items, wants_tokens)) -> repeater;
enddefine;

define updaterof XpmListItems(repeater, widget);
    lvars widget, repeater, in_tokens = false;
    if widget.isboolean then
        (repeater,widget) -> (repeater,widget, in_tokens);
    endif;
    Check_widget(widget);
    unless repeater.isprocedure then Listin(repeater) -> repeater endunless;
    Set_list_items(widget, repeater, in_tokens);
enddefine;

define XpmListTokenToItem = Get_token_item enddefine;

define XpmListItemToToken(widget, item);
    lvars widget, item;
    Check_widget(widget);
    Get_item_token(widget, list_item_assoc(widget), item);
enddefine;

define XpmViewListItem(widget, item);
    lvars widget, item, current_items, token = item;
    Check_widget(widget);
    list_item_assoc(widget) -> current_items;
    unless current_items then mishap(widget,1,'NO ITEMS TO VIEW'); endunless;
    unless token.islist_item then
        Get_item_token(widget, current_items, item) -> token;
        unless token then mishap(item,widget,2,'ITEM NOT IN LIST'); endunless;
        View_item(widget, token);
    endunless;
enddefine;

define XpmCurrentListItem(widget);
    lvars widget, current_items, item, wants_tokens = false, num;
    if widget.isboolean then widget -> (widget, wants_tokens) endif;
    Check_widget(widget);
    list_item_assoc(widget) -> current_items;
    Get_selected_items(widget, current_items, wants_tokens)() -> item;
    item /== termin and item;
enddefine;

define updaterof XpmCurrentListItem(item, widget);
    lvars item, widget, current_items;
    if widget.isboolean then (item,widget) -> (item,widget,); endif;
    unless item then
        []
    else
        XpmViewListItem(widget, item);
        [^item]
    endunless -> XpmSelectedListItems(widget);
enddefine;

define XpmSubscrListItems(n, widget) -> item;
    lvars n, widget, item, current_items, wants_token = false;
    if widget.isboolean then (n,widget) -> (n, widget, wants_token) endif;
    Check_widget(widget);
    list_item_assoc(widget) -> current_items;
    unless current_items then
        mishap(widget, 1, 'NO ITEMS IN LIST');
    endunless;
    subscrv(n, current_items) -> item;
    unless wants_token then li_item(item) -> item endunless;
enddefine;

define updaterof XpmSubscrListItems(item, n, widget);
    lvars item n widget current_items, tag, label;
    lconstant tag_ptr = writeable initlist_item_array(1);
    if widget.isboolean then (item,n,widget) -> (item,n,widget,); endif;
    Check_widget(widget);
    list_item_assoc(widget) -> current_items;
    unless current_items then
        mishap(widget, 1, 'NO ITEMS IN LIST');
    endunless;
    item sys_><'' -> label;
    (->XpmCoerceString(label)) -> tag;
    ;;; build new list_item wrapper for item, and register it.
    conslist_item("XmString", tag, tag, item, label)
                ->> XptRegister(tag) ->> tag;
    XmStringFree -> sys_destroy_action(tag);
    tag -> subscrv(n, current_items);
    tag -> tag_ptr(1);
    XmListReplaceItemsPos(widget, tag_ptr, 1, n);
enddefine;

define XpmNumListItems(widget);
    lvars widget, current_items;
    Check_widget(widget);
    list_item_assoc(widget) -> current_items;
    current_items and datalength(current_items) or 0;
enddefine;

constant xpm_listutils = true; /* for uses */

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 16 1993
        Uses Xm/xmListWidget instead of XptW*idgetSet
--- John Gibson, Sep 11 1992
        Changed to use XptVal
 */
