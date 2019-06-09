/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/AthenaWidgetSet.p
 > Purpose:         Minimal information for the Athena widget-set
 > Author:          Tom Khabaza, Aug 14 1990 (see revisions)
 > Documentation:   REF *ATHENA
 > Related Files:   LIB * XptWidgetSet
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

uses-now popxlib, Xaw, XptWidgetSet;

lconstant ws = XptNewWidgetSet("Athena");
XAW_EXLIBS -> ws("WidgetSetLibList");

if popmemlim - popmemused < 50000 then
    popmemlim + 50000 -> popmemlim; ;;; is this safe?
endif;

loadinclude XawCardinals.ph;
loadinclude XawConstants.ph;

lconstant wsmembers =
[   ;;; Xaw Widget Classes
    AsciiSinkObject
    AsciiSrcObject
    AsciiTextWidget
    BoxWidget
    ClockWidget             ;;; not in Athena Manual
    CommandWidget
    DialogWidget
    FormWidget
    GripWidget
    LabelWidget
    ListWidget
    LogoWidget              ;;; not in Athena Manual
    MailboxWidget           ;;; not in Athena Manual
    MenuButtonWidget
    PanedWidget
;;; ScrollWidget            ;;; made obsolete by ScrollbarWidget
    ScrollbarWidget
    SimpleMenuWidget
    SimpleWidget
    SmeBSBObject
    SmeLineObject
    SmeObject
    StripChartWidget
;;; TemplateWidget          ;;; not defined
    TextSinkObject
    TextSrcObject
    TextWidget
    ToggleWidget
;;; VPanedWidget            ;;; made obsolete by PanedWidget
    ViewportWidget
];

wsmembers -> ws("WidgetSetMembers");

lconstant AthenaDependencies = newproperty(
[[Cardinals ['XawCardinals.p' ^false]] ],60,false,"perm");

loadinclude XawConstants.ph;

;;; ATHENA DEPENDENCY TREE MANAGEMENT
define lconstant athena_widget_to_library_mapping(wsprefix, wcname) -> l;
    lvars wsprefix, wcname, l;
    XptGetWidgetDependencies(wsprefix, wcname,
                                    AthenaDependencies, "Athena") -> l;
    if lmember(wcname, wsmembers) then
        consword('xaw' sys_>< wcname) :: l -> l
    endif
enddefine;

athena_widget_to_library_mapping -> ws("WidgetSetFileMapping");
"Xaw" -> ws("WidgetSetPrefix");

global constant AthenaWidgetSet = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- Julian Clinton, Jan 17 1994
        Added some loadincludes so code using XptWidgetSet compiles okay.
--- John Gibson, Apr 17 1993
        All widgets now in Xaw/xaw*.p files, and WidgetSetFileMapping
        procedure loads them. Macros/typespecs in XawConstants.ph
--- John Gibson, Nov  3 1991
        includes xpt_generaltypes.ph
--- Andreas Schoter, Jul 19 1991
        Commented out TemplateWidgetSet references as it's not defined
--- Andreas Schoter, Jul 18 1991
        Removed general widget set dependency tree code to LIB *XptWidgetSet
        Added global constant for compatability with uses.
--- Andreas Schoter, Jul  4 1991
        Changed XawTextSearch type from ulong to long to get correct error
        code returned.
--- Andreas Schoter, Jul  2 1991
        Updated WidgetSetMembers, built dependency tree and added code for
        traversing it from LIB *MotifWidgetSet,
--- John Gibson, Jun 25 1991
        Corrected XT_LIBDIRS to XTLIBDIRS?
--- Robert John Duncan, Jun 11 1991
        New strategy for determining WidgetSetLibList
--- Jonathan Meyer, Oct 23 1990
        Fixed for new XptWidgetSet, and added widget set members list.
        Turned off any library loading.
--- Roger Evans, Oct 22 1990 revised liblist spoecification
--- James Goodlet, Sep 13 1990 - reorganised order and contents of liblist.
 */
