/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/xmListWidget.p
 > Purpose:         Motif widgetclass
 > Author:          John Gibson, Apr 14 1993
 > Documentation:   HELP * MOTIF
 > Related Files:   Xm/xm*Widget.p etc
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include xpt_coretypes.ph;

uses Xmgeneral;

XptLoadWidgetClass xmListWidget [^^XM_EXLIBS]
    xmListWidget    <- xmListWidgetClass,
;

define XmIsList =
    XtIsSubclass(% xmListWidget %)
enddefine;

XptPopLoadProcedures ''
    XmListAddItem(x,y,z) :void,
    XmListAddItems(w,x,y,z) :void,
    XmListAddItemUnselected(x,y,z) :void,
    XmListDeleteItem(x,y) :void,
    XmListDeleteItems(x,y,z) :void,
    XmListDeletePos(x,y) :void,
    XmListDeleteItemsPos(x,y,z) :void,
    XmListDeleteAllItems(x) :void,
    XmListReplaceItems(w,x,y,z) :void,
    XmListReplaceItemsPos(w,x,y,z) :void,
    XmListSelectItem(x,y,z) :void,
    XmListSelectPos(x,y,z) :void,
    XmListDeselectItem(x,y) :void,
    XmListDeselectPos(x,y) :void,
    XmListDeselectAllItems(x) :void,
    XmListSetPos(x,y) :void,
    XmListSetBottomPos(x,y) :void,
    XmListSetItem(x,y) :void,
    XmListSetBottomItem(x,y) :void,
    XmListSetAddMode(x,y) :void,
    XmListItemExists(x,y) :XptBoolean,
    XmListItemPos(x,y) :int,
    XmListGetMatchPos(w,x,y,z) :XptBoolean,
    XmListGetSelectedPos(x,y,z) :XptBoolean,
    XmListSetHorizPos(x,y) :void,
    XmCreateList(w,x,y,z) :XptWidget,
    XmCreateScrolledList(w,x,y,z) :XptWidget,
;

endexload_batch;
endsection;
