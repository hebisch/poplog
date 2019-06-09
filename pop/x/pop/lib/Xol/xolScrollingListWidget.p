/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/xolScrollingListWidget.p
 > Purpose:         ScrollingList widget definitions
 > Author:          Jonathan Meyer, Sep  2 1990 (see revisions)
 > Documentation:   HELP *OpenLook
 */
compile_mode :pop11 +strict;

include xpt_coretypes.ph;
include xdefs.ph;
include XolScroll.ph;

section;
exload_batch;

uses
    fast_xt_widgetclass,
    Xolgeneral,
;


/* Structures */

XptLoadWidgetClass xolScrollingListWidget [^^XOL_EXLIBS]
    xolScrollingListWidget <- scrollingListWidgetClass
;

shadowclass XpolListItemPtr [props OlListItemPtr] {
    XpolLILabelType :short,
    XpolLILabel     :XptString,
    XpolLIGlyph     :exptr,
    XpolLIAttr      :ulong,
;;; NAME OF "tag" FIELD CHANGES AND EXTRA FIELD FOR MOUSELESS
;;; SELECTION OF ITEMS AT OLIT 2.5.
#_IF XOL_VERSION < 2005
    XpolLITag       :exptr,
    :byte /* PADDING */
#_ELSE
    XpolLIUserData: exptr,
    XpolLIMnemonic: byte,
#_ENDIF
};

;;; FOR BACKWARD COMPATABILITY
syssynonym("XpOLListItemPtr_shadowkey", "XpolListItemPtr_shadowkey");
syssynonym("isXpOLListItemPtr", "isXpolListItemPtr");
syssynonym("importXpOLListItemPtr", "importXpolListItemPtr");
syssynonym("initXpOLListItemPtr", "initXpolListItemPtr");
syssynonym("refreshXpOLListItemPtr", "refreshXpolListItemPtr");
syssynonym("consXpOLListItemPtr", "consXpolListItemPtr");
syssynonym("destXpOLListItemPtr", "destXpolListItemPtr");
syssynonym("fillXpOLListItemPtr", "fillXpolListItemPtr");

syssynonym("XpOLLILabelType", "XpolLILabelType");
syssynonym("XpOLLILabel", "XpolLILabel");
syssynonym("XpOLLIGlyph", "XpolLIGlyph");
syssynonym("XpOLLIAttr", "XpolLIAttr");
#_IF XOL_VERSION >= 2005
    syssynonym("XpolLITag", "XpolLIUserData");
    syssynonym("XpOLLIUserData", "XpolLIUserData");
    syssynonym("XpOLLIMnemonic", "XpolLIMnemonic");
#_ENDIF
syssynonym("XpOLLITag", "XpolLITag");


#_IF XOL_VERSION > 2000

;;; AFTER VERSION 2 IT'S A FUNCTION
XptPopLoadProcedures '' [^^XOL_EXLIBS]
    OlListItemPointer(x) :exptr#XptImportOlListToken,
;

#_ELSE

;;; OTHERWISE TOKEN & ITEM ARE THE SAME
global constant
        OlListItemPointer = identfn;

#_ENDIF


/*
Pop-11 wrapper on the AddListItem - converts pointers to items into
actual C structures, which are passed on the call stack. See XtPoplog.c
*/


XptLoadProcedures ''
    lvars XpolAddListItem;

define XpolAddListItem(widget, parent, reference, item);
    lvars widget, parent, reference, item;

    unless fast_XtIsSubclass( XptCheckWidget(widget), xolScrollingListWidget)
    then
        mishap(widget,1,'OpenLook Scrolling List Widget Needed');
    endunless;

    XptLiveTypeCheck(item, "OlListItemPtr") -> item;

    if parent == false then 0
    else XptLiveTypeCheck(parent, "OlListToken")
    endif -> parent;

    if reference == false then 0
    else XptLiveTypeCheck(reference, "OlListToken")
    endif -> reference;

    exacc (4):OlListToken
        raw_XpolAddListItem(widget, parent, reference, item);
enddefine;


endexload_batch;
endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  8 1993
        Now xolScrollingListWidget.p
--- Adrian Howard, Jun 18 1992
        Added synonyms for the old Xpol identifiers so we have backwards
        compatability
--- Jonathan Meyer, Jun 16 1992
    Renamed things starting with XpOL -> Xpol (how they are documented).
--- Adrian Howard, Feb 21 1992
    o Removed XpOLListItem
    o XpOLListItemPtr shadowclass changed in 2 ways (caused by Sun changes)
        + -XpOLLITag- --> -XpOLLIUserData- at 2.5 (old field kept as a
            synonym)
        + new -XpOLLIMnemonic- field in 2.5
    o OlListItem typespec changed in 2 ways (caused by Sun changes)
        + "tag" field renamed "user_data" at 2.5
        + new "mnemonic" field at 2.5
--- Adrian Howard, Feb 20 1992 : Added -OlListItemPointer-
--- Adrian Howard, Nov  1 1991 :
    o renamed XpolListItem shadowclass XpOLListItemPtr and renamed fields as
      follows:
        XpolLILabelType -> XpOLLILabelType
        XpolLILabel -> XpOLLILabel
        XpolLIGlyph -> XpOLLIGlyph
        XpolLIAttr  -> XpOLLIAttr
        XpolLITag   -> XpOLLITag
        (XpolLIPad field removed)
    o added typespec:XpOLListItem
--- Andreas Schoter, Jul 15 1991
    Added global constant XolScrolling for compatibility with uses
--- Jonathan Meyer, May 29 1991
        added XpolAddListItem - to fix sun3 scrolling list procedure
        as in bugreport isl-fr.4319
--- Jonathan Meyer, Feb 15 1991
        renamed shadowclass structure to use Xpt naming conventions
--- Roger Evans, Feb  7 1991 changed XptPopString to XptString
--- Jonathan Meyer, Feb  6 1991 renamed XptOl Xpol
--- Roger Evans, Feb  3 1991 renamed XptGetDescriptor -> XptImportAny
--- Roger Evans, Jan 26 1991 changed to new shadowclass props format
--- Jonathan Meyer, Dec  5 1990
        Added shadow class and appl* typespecs
 */
